module CurryRTS

export Node, runCurry, makeChoice, setChoice, makeFree, setRHS, setConst,
       setForwardNode, bindVarNode, voidNode, rew_nf

##############################################################################
# Definition of graph nodes
mutable struct Node
  tag    :: UInt8       # 0: operation
                        # 1: forward
                        # 2: choice
                        # 3: fail
                        # 4: free
                        # 5: integer
                        # 6: float
                        # 7: char
                        # 8: partial function call
                        # 9: partial constructor call
                        # >=10: constructor
  value  :: Int64       # demanded argument (0: nothing demanded) for functions,
                        # choice identifier for choices,
                        # index of constructors or integer/char value
                        # missing arguments for partial calls
  otsk   :: Int64       # identifier of the owner task
  taskns :: Union{Nothing,Dict{Int64,Node}} # forward nodes for different tasks
  fcode                 # function code
  args   :: Array{Node} # arguments or forward node
  symbol :: Union{String,Float64} # string representation of node symbol
                                  # or value of a float
end

##############################################################################
# Definition of tasks
mutable struct Task
  control :: Node              # the root node to be evaluated by this task
  fprint  :: Dict{Int64,UInt8} # the fingerprint for this task
  tasknum :: Int64             # task number (for debugging)
  parents :: Array{Int32,1}    # the numbers of the parent tasks
end

##############################################################################

# Evaluate all (non-deterministic) results of a main operation.
function runCurry(mainname::String, maincode, withnf::Bool, interactive::Bool,
                  withbfs::Bool, onevalue::Bool, quiet::Bool)
  tasks::Vector{Task} = []
  fp = Dict{Int64,UInt8}()
  taskcounter = 0 # counter to enumerate new tasks
  nd = Node(0, 0, taskcounter, nothing, maincode, [], mainname)
  mainnd = withnf ? makeNormalForm(nd) : nd
  currtask = Task(mainnd, fp, taskcounter, [])
  createdtasks = 1
  finished = false
  if !quiet ; print("EVALUATE: "); printNode(currtask, mainnd); println() ; end
  while ! finished
    #println("EVALUATING TASK " * string(currtask.tasknum) * "...")
    result = hnfNode(currtask, taskcounter)
    if result.tag >= 3 # fail, free, constructor, integer,...
      if result.tag > 3 # no failing computation
        if !quiet ; print("RESULT: ") end
        printNode(currtask, result); println()
        finished = onevalue
        if !finished && interactive && ! isempty(tasks)
          answer = "?"
          while answer != "" && answer != "y" && answer != "n" && answer != "a"
            print("More values? [Y(es)/n(o)/a(ll)] ")
            answer = chomp(readline())
            if answer == "a"
              interactive = false
            elseif answer == "n"
              finished = true
            end
          end
        end
      end
      if isempty(tasks)
        finished = true
      else
        currtask = withbfs ? popfirst!(tasks) : pop!(tasks)
      end
    elseif result.tag == 2 # choice
      fp = currtask.fprint
      if haskey(fp, result.value)  # selection exists for this choice
        currtask.control = result.args[fp[result.value]]
      else
        #println("PULL TAB AT ROOT, NEW TASK:" * string(taskcounter+1))
        fp2 = copy(currtask.fprint)
        fp2[result.value] = 2
        newparents = vcat([currtask.tasknum], currtask.parents)
        currtask.fprint[result.value] = 1
        currtask.control = result.args[1]
        currtask.tasknum = taskcounter+1
        currtask.parents = newparents
        if withbfs
          push!(tasks, currtask)
          currtask = popfirst!(tasks)
        end
        push!(tasks, Task(result.args[2], fp2, taskcounter+2, newparents))
        taskcounter += 2
        createdtasks += 1
      end
    end
  end
  if !quiet ; println("NUMBER OF CREATED TASKS: " * string(createdtasks)) ; end
end

# The current task during the evaluation of hnfNode.
# It is used for makeFree and makeChoice if a free variable is instantiated
# by a narrowing step. It is initialized with a dummy value.
currentHnfTask = Task(Node(0,0,0,nothing,nothing,[],""),
                      Dict{Int64,UInt8}(), 0, [])

# Compute the head-normal form of the node contained in the control
# of the given task.
# The node is updated (maybe as a forward node) and the result node
# (which might be constructor-rooted or a choice or failure node) is returned.
function hnfNode(task :: Task, tskcnt :: Int64) :: Node
  global currentHnfTask
  currentHnfTask = task
  stack ::Vector{Node} = []
  cn = task.control
  while !isempty(stack) || cn.tag == 0 || cn.tag == 1
    #traceTask("ROOT", task, isempty(stack) ? cn : stack[1])
    if cn.tag == 4 && cn.otsk < task.tasknum && cn.taskns != nothing
      # follow the possible instantiation of a free variable:
      cn1 = followTaskForwardNode(task, cn)
      if cn1 != cn && !isempty(stack)
        # binding found, replace corresponding argument in top stack function:
        topfun = last(stack)
        if task.tasknum > topfun.otsk
          # insert forward node in topfun:
          if topfun.taskns == nothing
            #println("CREATE TASKNS")
            topfun.taskns = Dict{Int64,Node}()
          end
          fwdnode = Node(0,topfun.value,task.tasknum,nothing,topfun.fcode,
                         copy(topfun.args),topfun.symbol)
          topfun.taskns[task.tasknum] = fwdnode
          stack[end] = fwdnode
        end
        last(stack).args[topfun.value] = cn1
      end
      cn = cn1
    end
    if cn.tag >= 4      # free, constructor, integer,...
      cn = pop!(stack)

      if !isempty(stack)
        topfun = last(stack)
        if cn.otsk > topfun.otsk
          # insert forward node in topfun:
          if topfun.taskns == nothing
            #println("CREATE TASKNS")
            topfun.taskns = Dict{Int64,Node}()
          end
          fwdnode = Node(0,topfun.value,cn.otsk,nothing,topfun.fcode,
                         copy(topfun.args),topfun.symbol)
          topfun.taskns[cn.otsk] = fwdnode
          stack[end] = fwdnode
        end
        # set argument of top function (just to be safe for forward nodes)
        topfun = last(stack)
        topfun.args[topfun.value] = cn
      end

      cn.fcode(cn) # apply rewrite rule

    elseif cn.tag == 1  # forward node?
      #println("FORWARD")
      cn = cn.args[1]
      if !isempty(stack)
        topfun = last(stack)
        topfun.args[topfun.value] = cn
      end
    elseif cn.tag == 2  # choice node? follow choice or pull-tab step:
      fp = task.fprint
      if haskey(fp, cn.value)  # selection exists for this choice
        #println("FOLLOW CHOICE SELECTION")
        cn = cn.args[fp[cn.value]]
        if !isempty(stack)
          topfun = last(stack)
          if task.tasknum > topfun.otsk
            # insert forward node in topfun:
            if topfun.taskns == nothing
              #println("CREATE TASKNS")
              topfun.taskns = Dict{Int64,Node}()
            end
            fwdnode = Node(0,topfun.value,task.tasknum,nothing,topfun.fcode,
                           copy(topfun.args),topfun.symbol)
            topfun.taskns[task.tasknum] = fwdnode
            stack[end] = fwdnode
          end
          last(stack).args[topfun.value] = cn
        end
      else # pull tab step:
        fn = pop!(stack)
        args1 = copy(fn.args)
        args1[fn.value] = cn.args[1]
        args2 = copy(fn.args)
        args2[fn.value] = cn.args[2]
        #c1 = Node(0,fn.value,tskcnt+1,nothing,fn.fcode,args1,fn.symbol)
        #c2 = Node(0,fn.value,tskcnt+2,nothing,fn.fcode,args2,fn.symbol)
        c1 = Node(0,fn.value,fn.otsk,nothing,fn.fcode,args1,fn.symbol)
        c2 = Node(0,fn.value,fn.otsk,nothing,fn.fcode,args2,fn.symbol)
        fn.tag    = 2
        fn.value  = cn.value
        fn.taskns = nothing
        fn.fcode  = nothing
        empty!(fn.args); push!(fn.args,c1); push!(fn.args,c2)
        fn.symbol = "?"
        cn = fn
      end
    elseif cn.tag == 3  # failure node?
      empty!(stack)
    # final alternative: cn.tag == 0, i.e., operation
    else
      hasfwdnode = false
      if cn.otsk < task.tasknum && cn.taskns != nothing
        # check whether there is a forward node in this or some parent task:
        if haskey(cn.taskns, task.tasknum)
          hasfwdnode = true 
          cn = cn.taskns[task.tasknum]
        else
          for i in 1:length(task.parents)
            if haskey(cn.taskns, task.parents[i])
              hasfwdnode = true 
              cn = cn.taskns[task.parents[i]]
              break
            end
          end
        end
      end
      if hasfwdnode
        #println("FOLLOW RESULT FORWARD NODE")
        if !isempty(stack)
          topfun = last(stack)
          topfun.args[topfun.value] = cn
        end
      elseif cn.value == 0 # no argument demanded?
        cn.fcode(cn) # apply rewrite rule
      else
        push!(stack, cn)   # push current function on stack
        cn = cn.args[cn.value] # proceed with demanded argument
      end
    end
  end
  #traceTask("HNF", task, cn)
  return cn
end

##############################################################################
# Auxiliary operations for dealing with choice nodes

# The global counter for fresh choice identifiers
freshChoiceId = 0

# Create a choice node with a fresh choice identifier for a given task
# identifier.
function makeChoice(otsk, l, r) :: Node
  global freshChoiceId
  freshChoiceId += 1
  return Node(2,freshChoiceId,otsk,nothing,nothing,[l,r],"?")
end

# replace argument node by a choice node with a fresh choice identifier
function setChoice(root :: Node, l :: Node, r :: Node)
  global freshChoiceId
  freshChoiceId += 1
  root.symbol = "?"
  root.tag    = 2
  root.value  = freshChoiceId
  empty!(root.args); push!(root.args,l); push!(root.args,r)
end

##############################################################################
# Auxiliary operations

# Node representing a void value. Used to initialize Node variables.
function voidNode(otsk)
  return Node(10, 0, otsk, nothing, nothing, [], "()")
end

# Create a free variable node
function makeFree(otsk) :: Node
  return Node(4, 0, otsk, nothing, nothing, [], "FREE")
end

# Set the first argument node (which is the root of a redex)
# to the right-hand side provided as the remaining arguments.
function setRHS(root :: Node, ntag, nvalue, nfcode, nargs, nsymbol)
  root.tag    = ntag
  root.value  = nvalue
  root.fcode  = nfcode
  root.args   = nargs
  root.symbol = nsymbol
end

# Set the first argument node (which is the root of a redex)
# to a constant value provided as the remaining arguments.
function setConst(root :: Node, ntag, nvalue)
  root.tag     = ntag
  root.value   = nvalue
end

# Set the first argument node to a forward node.
function setForwardNode(root :: Node, fnode :: Node)
  root.tag  = 1
  empty!(root.args); push!(root.args,fnode)
end

# Bind the first argument node (a free variable) to another node.
# Note that this binding is made only for the current task.
function bindVarNode(varnode :: Node, n :: Node)
  global currentHnfTask
  if currentHnfTask.tasknum > varnode.otsk
    # insert forward node in varnode:
    if varnode.taskns == nothing
      #println("CREATE TASKNS FOR FREE VARIABLE")
      varnode.taskns = Dict{Int64,Node}()
    end
    varnode.taskns[currentHnfTask.tasknum] = n
  else
    setForwardNode(varnode, n)
  end
end

##############################################################################
# ADDITIONAL STUFF TO SUPPORT THE REPL:
##############################################################################
# Normal form computation.

# Wrap a node with the `normalForm` operation.
function makeNormalForm(root :: Node)
  return Node(0,1,root.otsk,nothing,rew_nf,[root],"normalForm")
end

# Operation to compute the normal form of its argument.
# Implemented by wrapping all arguments with `normalForm` and
# evaluating successively all arguments (by operation `rew_nfArgs`).
function rew_nf(root :: Node)
  x = root.args[1]
  if x.tag == 4 # free variable: create forward node
    root.tag  = 1
    empty!(root.args); push!(root.args,x)
  elseif x.tag < 10 || length(x.args) == 0 # no argument evaluation necessary
    root.tag    = x.tag
    root.symbol = x.symbol
    root.value  = x.value
    root.otsk   = x.otsk
    root.taskns = x.taskns
    root.fcode  = nothing
    root.args   = x.args
  else
    root.value = 1
    root.fcode = rew_nfArgs
    root.args  = vcat(map(makeNormalForm,x.args), [x]) # remember constructor
  end
end

function rew_nfArgs(root :: Node)
  darg = root.value + 1
  if darg == length(root.args) # all constructor arguments evaluated
    cterm       = pop!(root.args)
    root.tag    = cterm.tag
    root.symbol = cterm.symbol
    root.value  = cterm.value
    root.otsk   = cterm.otsk
    root.taskns = cterm.taskns
    root.fcode  = nothing
  else
    root.value = darg
  end
end

##############################################################################
# Auxiliaries for printing results and tracing

# Print a graph node as a term in a task, i.e., fingerprints are considered
# when printing choice nodes.
function printNode(tsk:: Task, n :: Node)
  hasfwdnode = false
  if (n.tag == 0 || n.tag == 4) && n.otsk < tsk.tasknum && n.taskns != nothing
    n1 = followTaskForwardNode(tsk, n)
    if n1 != n
      n = n1
      hasfwdnode = true
    end
  end
  if hasfwdnode
    # follow result forward node:
    print("%") ; printNode(tsk, n)
  elseif n.tag == 1 # forward node
    print("^") ; printNode(tsk, n.args[1])
  elseif n.tag == 2 # choice node
    fp = tsk.fprint
    if haskey(fp, n.value)
      printNode(tsk, n.args[fp[n.value]])
    else
      print("(")
      printNode(tsk, n.args[1])
      print(" ?" * string(n.value) * " ")
      printNode(tsk, n.args[2])
      print(")")
    end
  elseif n.tag == 3 # fail node
    print("!")
  elseif n.tag == 4 # free node
    #print("FREE")
    #print("_x" * string(objectid(n)))
    print("_x" * string(objectid(n))[end-3:end])
    #printNodeSymbol(n)
  elseif n.tag == 5 # integer node
    print(string(n.value))
  elseif n.tag == 7 # character node
    if n.value > 31
      print("'" * Char(n.value) * "'")
    else
      print("(char " * string(n.value) * ")")
    end
  elseif length(n.args) == 0
    printNodeSymbol(n)
  elseif length(n.args) == 2 && !isletter(n.symbol[1]) # infix operator
    print("(")
    printNode(tsk,n.args[1])
    print(" ") ; printNodeSymbol(n) ; print(" ")
    printNode(tsk,n.args[2])
    print(")")
  else
    print("(")
    printNodeSymbol(n)
    for i = 1 : length(n.args)
      print(" ") ; printNode(tsk,n.args[i])
    end
    print(")")
  end
end

function followTaskForwardNode(tsk:: Task, n :: Node)
  if haskey(n.taskns, tsk.tasknum)
    n = n.taskns[tsk.tasknum]
  else
    for i in 1:length(tsk.parents)
      if haskey(n.taskns, tsk.parents[i])
        n = n.taskns[tsk.parents[i]]
        break
      end
    end
  end
  return n
end

# Print a node symbol
function printNodeSymbol(n :: Node)
  #print(n.symbol * (n.tag == 0 ? "[" * string(objectid(n)) * "]" : ""))
  #print(n.symbol * (n.tag == 0 ? "[" * string(n.otsk) * "]" : ""))
  print(n.symbol)
end

# Show a finger print
function showFPrint(fp :: Dict{Int64,UInt8}) :: String
  s = "["
  for (index, value) in pairs(fp)
    s = s * string(index) * "=>" * (value==1 ? "L" : "R") * " "
  end
  return rstrip(s) * "]"
end

function parentsString(p) :: String
  s = ""
  for i in 1:length(p)
    s = s * "," * string(p[i])
  end
  return s
end

function traceTask(ctxt :: String, task :: Task, cn :: Node)
  for i = 1:length(task.parents)
    print("  ")
  end
  print(ctxt * "[" * string(task.tasknum) * parentsString(task.parents) * "]")
  print(showFPrint(task.fprint))
  print(": ")
  printNode(task,cn)
  println()
end

##############################################################################

end
