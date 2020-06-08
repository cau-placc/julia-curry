module PulltabOnlyRTS

export Node, runCurry, makeChoice, setChoice, makeFree,
       setRHS, setConst, setForwardNode, rew_apply, voidNode, rew_nf

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
  symbol :: Union{String,Float64} # string representation of node symbol
                                  # or value of a float
  value  :: Int64       # demanded argument (0: nothing demanded) for functions,
                        # choice identifier for choices,
                        # index of constructors or integer/char value
                        # missing arguments for partial calls
  fcode                 # function code
  args   :: Array{Node} # arguments or forward node
end

##############################################################################
# Definition of tasks
mutable struct Task
  control :: Node              # the root node to be evaluated by this task
  fprint  :: Dict{Int64,UInt8} # the fingerprint for this task
  tasknum :: Int64             # task number (for debugging)
end

##############################################################################

# Evaluate all (non-deterministic) results of a main operation.
function runCurry(mainname::String, maincode, withnf::Bool, interactive::Bool,
                  withbfs::Bool, onevalue::Bool, quiet::Bool)
  tasks::Vector{Task} = []
  fp = Dict{Int64,UInt8}()
  taskcounter = 1
  nd = Node(0, mainname, 0, maincode, [])
  mainnd = withnf ? makeNormalForm(nd) : nd
  currtask = Task(mainnd, fp, taskcounter)
  finished = false
  if !quiet ; print("EVALUATE: "); printNode(currtask, mainnd); println() ; end
  while ! finished
    result = hnfNode(currtask)
    #println("EVALUATING TASK " * string(currtask.tasknum) * "...")
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
        taskcounter += 1
        #println("PULL TAB AT ROOT, NEW TASK:" * string(taskcounter))
        fp2 = copy(currtask.fprint)
        fp2[result.value] = 2
        currtask.fprint[result.value] = 1
        currtask.control = result.args[1]
        if withbfs
          push!(tasks, currtask)
          currtask = popfirst!(tasks)
        end
        push!(tasks, Task(result.args[2],fp2,taskcounter))
      end
    end
  end
  if !quiet ; println("NUMBER OF CREATED TASKS: " * string(taskcounter)) ; end
end


# Compute the head-normal form of the node contained in the control
# of the given task.
# The node is updated (maybe as a forward node) and the result node
# (which might be constructor-rooted or a choice or failure node) is returned.
function hnfNode(task :: Task) :: Node
  stack ::Vector{Node} = []
  cn = task.control
  while !isempty(stack) || cn.tag == 0 || cn.tag == 1
    #traceTask("ROOT", task, isempty(stack) ? cn : stack[1])
    if cn.tag >= 4              # free, constructor, integer,...
      cn = pop!(stack)
      cn.fcode(cn) # apply rewrite rule
    elseif cn.tag == 1  # forward node?
      #println("FORWARD")
      cn = cn.args[1]
      if !isempty(stack)
        topfun = last(stack)
        topfun.args[topfun.value] = cn
      end
    elseif cn.tag == 2  # choice node? pull-tab step:
      fn = pop!(stack)
      args1 = copy(fn.args)
      args1[fn.value] = cn.args[1]
      args2 = copy(fn.args)
      args2[fn.value] = cn.args[2]
      c1 = Node(0,fn.symbol,fn.value,fn.fcode,args1)
      c2 = Node(0,fn.symbol,fn.value,fn.fcode,args2)
      fn.tag    = 2
      fn.value  = cn.value
      fn.fcode  = nothing
      empty!(fn.args); push!(fn.args,c1); push!(fn.args,c2)
      fn.symbol = "?"
      cn = fn
    elseif cn.tag == 3  # failure node?
      empty!(stack)
    elseif cn.value == 0
      cn.fcode(cn) # apply rewrite rule
    else
      push!(stack,cn) # push current function on stack
      cn = cn.args[cn.value] # proceed with demanded argument
    end
  end
  #traceTask("HNF", task, cn)
  return cn
end

##############################################################################
# Auxiliary operations for dealing with choice nodes

# The global counter for fresh choice identifiers
freshChoiceId = 0

# Create a choice node with a fresh choice identifier
function makeChoice(l, r) :: Node
  global freshChoiceId
  freshChoiceId += 1
  return Node(2,"?",freshChoiceId,nothing,[l,r])
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
# Auxiliary operations for dealing with free variables

# Node representing a void value. Used to initialize Node variables.
voidNode = Node(10, "()", 0, nothing, [])

# Create a free variable node
function makeFree(tski) :: Node
  return Node(4, "free", 0, nothing, [])
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

# Set the contents of the first argument node.
function setForwardNode(root :: Node, fnode :: Node)
  root.tag  = 1
  empty!(root.args); push!(root.args,fnode)
end

##############################################################################
# Normal form computation.

# Wrap a node with the `normalForm` operation.
function makeNormalForm(root :: Node)
  return Node(0,"normalForm",1,rew_nf,[root])
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
  if n.tag == 1 # forward node
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
    print("FREE")
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

# Print a node symbol
function printNodeSymbol(n :: Node)
  #print(n.symbol * (n.tag == 0 ? "[" * string(objectid(n)) * "]" : ""))
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

function traceTask(ctxt :: String, task :: Task, cn :: Node)
  print(ctxt * "[" * string(task.tasknum) * "]")
  #print(showFPrint(task.fprint))
  print(": ")
  printNode(task,cn)
  println()
end

##############################################################################

end
