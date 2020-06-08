module BacktrackRTS

export Node, runCurry, trailNode, makeChoice, setChoice, makeFree, makeNode,
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
  value  :: Int64       # demanded argument (0: nothing demanded) for functions,
                        # choice identifier for choices,
                        # index of constructors or integer/char value
                        # missing arguments for partial calls
  tski   :: Int64       # identifier to remember creation time (choice point)
  fcode                 # function code
  args   :: Array{Node} # arguments or forward node
  symbol :: Union{String,Float64} # string representation of node symbol
                                  # or value of a float
end

##############################################################################
# Auxiliaries for implementing backtracking

# Elements stored on the backtrack stack
mutable struct ChoicePoint
  choicenum :: Int64        # number of this choice point
  stack     :: Vector{Node} # the stack contents at the choice point
  trail     :: Vector{Tuple{Node,Node}} # the trail containing all node changes
                                        # to be undone on backtracking
end

# The global backtrack stack. Initially, it contains a void choice point.
backtrackstack = [ChoicePoint(0, [], [])]

# This global variable contains the value of `last(backtrackstack).choicenum`.
# Used for faster access since its value is used at various places.
currentchoicenum = 0

# Counter for trail events (just for statistics)
trailcounter = 0

# Push a node together with a copy of its contents on the trail,
# but only if it has been created before the current choice point.
# The latter condition avoids unnecessary trailing of deterministic
# computations.
function trailNode(n :: Node)
  global trailcounter
  global currentchoicenum

  if n.tski < currentchoicenum
    #print("TRAIL NODE: "); printNode(n); println()
    trailcounter += 1
    cn = Node(n.tag, n.value, n.tski, n.fcode, copy(n.args), n.symbol)
    currentcp = last(backtrackstack)
    push!(currentcp.trail,(n,cn))
  end
end

##############################################################################
# Evaluate all (non-deterministic) results of a main operation.
function runCurry(mainname::String, maincode, withnf::Bool, interactive::Bool,
                  withbfs::Bool, onevalue::Bool, quiet::Bool)
  global currentchoicenum = 0
  global trailcounter     = 0
  global backtrackstack   = [ChoicePoint(0, [], [])]

  stack ::Vector{Node} = []
  numchoices = 0 # number of created choice points
  nd = Node(0, 0, 0, maincode, [], mainname)
  mainnd = withnf ? makeNormalForm(nd) : nd
  finished = false
  if !quiet ; print("EVALUATE: "); printNode(mainnd); println() ; end
  cn = mainnd
  while ! (finished || (isempty(stack) && isempty(backtrackstack)))
    #traceEval(isempty(stack) ? cn : stack[1])
    if cn.tag >= 4              # free, constructor, integer,...
      if isempty(stack)
        if !quiet ; print("RESULT: ") end
        printNode(cn); println()
        finished = onevalue
        if !finished && interactive
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
        cn.tag = 3 # initiate backtracking
      else
        cn = pop!(stack)
        cn.fcode(cn)              # apply rewrite rule
      end

    elseif cn.tag == 1  # forward node?
      #println("FORWARD")
      cn = cn.args[1]
      if !isempty(stack)
        topfun = last(stack)
        trailNode(topfun)
        topfun.args[topfun.value] = cn
      end

    elseif cn.tag == 2  # choice node? create choice point and follow left:
      currentchoicenum += 1
      #println("CREATE CHOICE POINT " * string(currentchoicenum))
      # create choice point:
      savedstack = copy(stack)
      push!(savedstack, cn)
      newcp = ChoicePoint(currentchoicenum, savedstack, [])
      push!(backtrackstack, newcp)
      numchoices += 1
      setForwardNode(cn,cn.args[1])
      cn = cn.args[1]
      if !isempty(stack)
        topfun = last(stack)
        trailNode(topfun)
        topfun.args[topfun.value] = cn
      end

    elseif cn.tag == 3  # failure node?
      cp = pop!(backtrackstack)
      if length(cp.stack) == 0 # last choice point?
        stack = []
      else
        #println("BACKTRACK: RESTORE CHOICE POINT " * string(cp.choicenum))
        currentchoicenum = last(backtrackstack).choicenum
        # restore trailed changes of choice point:
        while length(cp.trail) > 0
          tn = pop!(cp.trail)
          tn1 = tn[1]
          tn2 = tn[2]
          tn1.tag    = tn2.tag
          tn1.value  = tn2.value
          tn1.tski   = tn2.tski
          tn1.fcode  = tn2.fcode
          tn1.args   = tn2.args
          tn1.symbol = tn2.symbol
          #print("UNTRAIL NODE: "); printNode(tn1); println()
          #traceEval(isempty(stack) ? cn : stack[1])
        end
        stack = cp.stack
        cn = pop!(stack)
        #println("GO RIGHT IN CHOICE POINT " * string(cp.choicenum))
        #traceEval(isempty(stack) ? cn : stack[1])
        setForwardNode(cn, cn.args[2])
        cn = cn.args[1]
        if !isempty(stack)
          topfun = last(stack)
          trailNode(topfun)
          topfun.args[topfun.value] = cn
        end
      end

    # final alternative: cn.tag == 0, i.e., operation
    else
      if cn.value == 0 # no argument demanded?
        cn.fcode(cn) # apply rewrite rule
      else
        push!(stack,cn)   # push current function on stack
        cn = cn.args[cn.value] # proceed with demanded argument
      end
    end
  end
  if !quiet
    println("NUMBER OF TRAILED ELEMENTS: " * string(trailcounter))
    println("NUMBER OF CREATED CHOICE POINTS: " * string(numchoices))
  end
end

##############################################################################
# Auxiliary operations for dealing with choice nodes

# Create a choice node with a fresh choice identifier
function makeChoice(tski, l, r) :: Node
  return Node(2, 0, tski, nothing, [l,r], "?")
end

# Replace argument node by a choice node
function setChoice(root :: Node, l :: Node, r :: Node)
  trailNode(root)
  root.symbol = "?"
  root.tag    = 2
  root.value  = 0
  empty!(root.args); push!(root.args,l); push!(root.args,r)
end

##############################################################################
# Auxiliary operations

# Node representing a void value. Used to initialize Node variables.
function voidNode(tski)
  return Node(10, 0, tski, nothing, [], "()")
end

# Create a free variable node
function makeFree(tski) :: Node
  return makeNode(4, 0, nothing, [], "free")
end

# Replace first argument node by content of second argument node.
# Used to instantiate free variable nodes
function makeNode(ntag, nvalue, nfcode, nargs, nsymbol)
  global currentchoicenum
  return Node(ntag, nvalue, currentchoicenum, nfcode, nargs, nsymbol)
end

# Set the first argument node (which is the root of a redex)
# to the right-hand side provided as the remaining arguments.
function setRHS(root :: Node, ntag, nvalue, nfcode, nargs, nsymbol)
  global currentchoicenum
  trailNode(root)
  root.tski    = currentchoicenum
  root.tag     = ntag
  root.value   = nvalue
  root.fcode   = nfcode
  root.args    = nargs
  root.symbol  = nsymbol
end

# Set the first argument node (which is the root of a redex)
# to a constant value provided as the remaining arguments.
function setConst(root :: Node, ntag, nvalue)
  global currentchoicenum
  trailNode(root)
  root.tski    = currentchoicenum
  root.tag     = ntag
  root.value   = nvalue
end

# Set the first argument node (which is the root of a redex)
# to a float value provided as the remaining arguments.
function setFloatConst(root :: Node, fvalue)
  global currentchoicenum
  trailNode(root)
  root.tski    = currentchoicenum
  root.tag     = 6
  root.symbol  = fvalue
end

# Set the first argument node to a forward node
function setForwardNode(root :: Node, fnode :: Node)
  trailNode(root)
  root.tag  = 1
  empty!(root.args); push!(root.args,fnode)
end

##############################################################################
# Normal form computation.

# Wrap a node with the `normalForm` operation.
function makeNormalForm(root :: Node)
  return makeNode(0, 1, rew_nf, [root], "normalForm")
end

# Operation to compute the normal form of its argument.
# Implemented by wrapping all arguments with `normalForm` and
# evaluating successively all arguments (by operation `rew_nfArgs`).
function rew_nf(root :: Node)
  trailNode(root)
  x = root.args[1]
  if x.tag == 4 # free variable: create forward node
    root.tag  = 1
    empty!(root.args); push!(root.args,x)
  elseif x.tag < 10 || length(x.args) == 0 # no argument evaluation necessary
    root.tag    = x.tag
    root.symbol = x.symbol
    root.value  = x.value
    root.tski   = x.tski
    root.fcode  = nothing
    root.args   = x.args
  else
    root.value = 1
    root.fcode = rew_nfArgs
    root.args  = vcat(map(makeNormalForm,x.args), [x]) # remember constructor
  end
end

function rew_nfArgs(root :: Node)
  trailNode(root)
  darg = root.value + 1
  if darg == length(root.args) # all constructor arguments evaluated
    cterm       = pop!(root.args)
    root.tag    = cterm.tag
    root.value  = cterm.value
    root.tski   = cterm.tski
    root.fcode  = nothing
    root.symbol = cterm.symbol
  else
    root.value = darg
  end
end

##############################################################################
# Auxiliaries for printing results and tracing

# Print a graph node as a term.
function printNode(n :: Node)
  if n.tag == 1 # forward node
    print("^") ; printNode(n.args[1])
  elseif n.tag == 2 # choice node
    print("(")
    printNode(n.args[1])
    print(" ?" * string(n.value) * " ")
    printNode(n.args[2])
    print(")")
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
    printNode(n.args[1])
    print(" ") ; printNodeSymbol(n) ; print(" ")
    printNode(n.args[2])
    print(")")
  else
    print("(")
    printNodeSymbol(n)
    for i = 1 : length(n.args)
      print(" ") ; printNode(n.args[i])
    end
    print(")")
  end
end

# Print a node symbol
function printNodeSymbol(n :: Node)
  #print(n.symbol * (n.tag == 0 ? "[" * string(objectid(n)) * "]" : ""))
  #print(n.symbol * (n.tag == 0 ? "[" * string(n.tski) * "]" : ""))
  print(n.symbol)
end

function traceEval(cn :: Node)
  print("ROOT: ")
  printNode(cn)
  println()
end

##############################################################################

end
