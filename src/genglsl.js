(function() {
    eval(Narcissus.definitions.consts);
    eval(RiverTrail.definitions.consts);

    const tokens = RiverTrail.definitions.tokens;

    var varyingTypes = {};
    varyingTypes.__proto__ = null;

    var reportError = RiverTrail.Helper.reportError;
    var reportBug = RiverTrail.Helper.reportBug;

    function parseGladderArrayDecl(decl) 
    {
        var match = decl.match(/vec([2-4])\[(\d)+\]/);
        if (match) {
            return { length: parseInt(match[2]), rank: parseInt(match[1]) };
        } else return null;
    }

    function isGlMatrixMatSingleton(str)
    {
        return str === "mat4" || str === "mat3" || str === "mat2";
    }

    function isGlMatrixVecSingleton(str)
    {
        return str === "vec4" || str === "vec3" || str === "vec2";
    }

    function isGlMatrixSingleton(str) {
        return isGlMatrixVecSingleton(str) || isGlMatrixMatSingleton(str);
    }

    function emitAttributeDecls(args)
    {
        var s = "";
        for (var k in args.attributes) {
            s += "attribute " + args.attributes[k] + " " + k + ";\n"; 
        }
        return s;
    }

    function emitUniformDecls(args)
    {
        var s = "";
        for (var k in args.uniforms) { // FIXME sanitize
            var decl = args.uniforms[k];
            var result = parseGladderArrayDecl(decl);
            if (result) {
                // FIXME vec?
                var vecdec = "uniform vec" + result.rank + " " + k + "[" + result.length + "]";
                s += vecdec + ";\n";
            } else {
                s += "uniform " + decl + " " + k + ";\n"; 
            }
        }
        return s;
    }

    function emitVaryings() 
    {
        var s = "";
        for (var key in varyingTypes) {
            var type = varyingTypes[key];
            s += "varying " + type.slangTypedName(key) + ";\n";
        }
        return s;
    }


    function genReturn (ast) {
        //"use strict"; // seems to trigger a timing-dependent behavior in Chrome
        // go over the literal, add varyings
        function genArrayRet(name, ast) {
            var iVar = varNameGen("ret");
            // FIXME we are reevaluating the expression?, make sure this is just an identifier?
            //if (ast.value.type != IDENTIFIER) debugger;
            var res = glslExpression(ast.value);
            // FIXME this is just one depth of array, but that's all that's supported, but what if it's an array of mat?
            return "for (int " + iVar + " = 0; " + iVar + " < " + ast.typeInfo.properties.shape[0] + "; " + iVar + " ++) { " + name + "[" + iVar + "] = " + RENAME(res) + "[" + iVar + "];} ";
        }


        if (ast.typeInfo.isSlangArray()) {
            return genArrayRet("_retVal", ast);
        }

        if (ast.value) {
            var s = "" ;
            if (ast.value.type == OBJECT_INIT)
            {

                for (var i = 0; i < ast.value.children.length; i++) 
                {
                    var init = ast.value.children[i];
                    var name = init.children[0].value;
                    varyingTypes[name] = init.children[1].typeInfo;
                    if (varyingTypes[name].isSlangArray()) {
                        // element-by-element assignmment
                        s += genArrayRet(name, init.children[1]) + "\n";
                    } else {
                    //console.log("inferred type of varyingTypes[" + name + "] is " + init.children[1].typeInfo);
                        s = s + "(" + name +  "= " + glslExpression(init.children[1]) + ");\n"; // no ; because ASSIGN is an expression!
                    }
                }
                return s;
            } else {
                // FIXME is it finished?
                s = s + "return " + glslExpression(ast.value);
                return s;
            }
        } else {
            return "return FIXME<" + Narcissus.decompiler.pp(ast) + ">";
        }
    }

    var renameEnv = {};
    renameEnv.__proto__ = null;

    // FIXME not unique
    var varNameGen = function () {
        var counter = 0;

        return function varNameGen(postfix) {
            return "v" + (counter++) + "_" + (postfix || "nameless");
        };
    }();

    
    var RENAME = function (s) { 
        if(renameEnv[s] === undefined) {
            // FIXME double check that gl_ is special?
            return s.indexOf("gl_") == 0 ? s : RENAME.prefix + s;
        } else {
            return renameEnv[s];
        }
    };
    RENAME.prefix = "rt_";


    function stripCasts(ast) {
        if (ast.type === CAST) {
            return stripCasts(ast.children[0]);
        } else {
            return ast;
        }
    }

    
    function glslStatement(ast) {
        //"use strict"; // seems to trigger a timing-dependent behavior in Chrome
        
        var s = "";
        if (!ast) {
            return "";
        }
        if (!(ast instanceof Object)) {
            return ast;
        }
        
        if (ast.parenthesized) {
            s += "(";
        }
        
        
        // From SEH There is a whole lot of code in here that has not be changed over to the genOCL code but at this point
        // it only compiles the default loader program below.
        // Using combine for a combinator.
        // var pa = new ParallelArray([1,2,3]);
        // var foo = function foo(iv) { return this.get(iv); };

        switch (ast.type) {
            
        case SCRIPT:
            s=s+"SCRIPT TBD ";
            // retrieve the type environment for local bindings
            
            // retrieve all the function environments for other function in case we see a call.
            
            // retrieve the symbols ??
            
            // add all local variable declarations to environment to shadow old
            // ones from previous scopes
            //      ast.varDecls.forEach(function (name) { doSomething(name); });
            
            // add all locally declared functions to the environment
            // strictly speaking they are not variable bindings yet they can shadow variables and be shadowed
            // by variables so we disallow these
            //      ast.funDecls.forEach(function (f) { doSomething(f)});
            
            // fallthrough to deal with the implicit SCRIPT block.
        case BLOCK:
            ast.children.forEach(function (ast) { glslStatement(ast); });
            break;
            
            //
            // statements
            //
        case FUNCTION:
            // this is not an applied occurence but the declaration, so we do not do anything here
            // functions are picked up from the body node earlier on

            break;
        case RETURN:
            // FIXME check compatibility of all return values? in typeinferece?
            s += genReturn(ast);
            break;
        case FOR:
            var setupExpr = glslExpression(ast.setup);
            var loopIndexType = ast.setup.children[0].typeInfo.OpenCLType;
            var constInit =  ast.setup.children[0].initializer; // FIXME check that it's a literal
            // if (constInit.typeInfo instanceof TLiteral) ...
            var existingName = ast.setup.children[0].value;
            var newName = varNameGen("loop_"); // FIXME non-clashing name?
            s += "for ( " + loopIndexType + " " + newName + " = " + glslExpression(constInit) + "; ";
            var saved = renameEnv;
            try {
                renameEnv = {};
                renameEnv[existingName] = newName;
                renameEnv.__proto__ = saved;
                
                s +=  glslExpression(ast.condition) + ";" 
                    + glslExpression(ast.update) + ") ";
                s += glslStatements(ast.body, RENAME.prefix + existingName + " = " + newName + ";"); // write back from new var to old var
            } finally {
                renameEnv = saved;
            }
            break;
        case WHILE:
            s += "while ( "+glslExpression(ast.condition)+") ";
            s += glslStatements(ast.body);
            break;
        case DO:
            s += "do " + glslStatements(ast.body)+" while ("+glslExpression(ast.condition)+");";
            break;
        case IF:
            s += "if ("+glslExpression(ast.condition)+") {";
            s += glslStatements(ast.thenPart) + "} ";
            if (ast.elsePart) {
                s += "else {"+glslStatements(ast.elsePart) + "}";
            }
            break;
        case SEMICOLON:
            if (ast.expression) {
                s += glslStatement(ast.expression)+";";
            }
            break;
        case CONST:
            break; // const initialziers are handled at declaration time
        case VAR:
            s = s + ast.children.reduce(function (prev, ast) {
                if (ast.initializer) { // CONSTs are handled at initialization
                    if (ast.initializer.typeInfo.isPseudoType()) {
                        // we're propagating a pseudotype, codegen will go directly do the appropriate individual vars
                        // no code
                        return prev;
                    }

                    switch (ast.type) {
                    case IDENTIFIER:
                        // simple case of a = expr
                        // initialisation  assignments are separated by commata (which is valid C) to make them work in for statement initialisers.
                        if (prev !== "") {
                            prev += ", ";
                        }
                        ast.initializer.assignedIdentifier = ast.value;
                        if (ast.initializer.type == ARRAY_INIT) {
                            prev += glslExpression(ast.initializer); 
                        } else  {
                            if (ast.initializer.typeInfo.isSlangArray()) {
                                // nope
                            } else {
                                prev += RENAME(ast.value) + " = ";
                            }
                            prev += glslExpression(ast.initializer); 
                        }
                        
                        break;
                    default:
                        reportBug("unhandled lhs in var/const");
                        break;
                    }   
                }
                return prev;
            }, "");
            break;
        case ASSIGN:
            // children[0] is the left hand side, children[1] is the right hand side.
            // both can be expressions. 
            if (ast.children[1].typeInfo.isPseudoType()) {
                // we're propagating a pseudotype, codegen will go directly do the appropriate individual vars
                // no code

                break;
            }
            switch (ast.children[0].type) {
            case IDENTIFIER:
                // simple case of a = expr
                if (ast.allocatedMem) {
                    //throw new Error("a memcopy would be required to compile this code.");
                    var s_tmp = ""; var s_decl = "";
                    var sourceShape = ast.children[1].typeInfo.getOpenCLShape();
                    var sourceType = ast.children[1].typeInfo.OpenCLType;
                    var maxDepth = sourceShape.length;
                    var sourceAddressSpace = (ast.children[1].typeInfo.getOpenCLAddressSpace() == "__global" ? "__global": "");
                    if(!(ast.children[1].typeInfo.isScalarType()) && maxDepth >= 1) {
                        verboseDebug && console.log("Doing copy assignment to value ", ast.children[0].value, " from shape ", sourceShape);
                        var source_tmp_name = "tmp_" + ast.memBuffers.list[0];
                        s_decl += "/* Copying Assignment */ " + sourceAddressSpace + " " + sourceType + " " + source_tmp_name + " = " + glslExpression(ast.children[1]) + ";" ;
                        s_tmp += "(";
                        var post_parens = ""; 
                        var redu = 1; var rhs = ""; var lhs = ""; post_parens = ")";
                        for(var i = 0 ; i < maxDepth; i++) {
                            for(var j = 0; j < sourceShape[i]*redu; j++) {
                                if(i===maxDepth-1) {
                                    lhs = "(" + getPointerCast(i, maxDepth, ast.typeInfo.OpenCLType) +
                                        ast.memBuffers.list[i] + ")" + "[" + j + "]";
                                    var n = j; var idx = "";
                                    for(var k = maxDepth-1; k >=0; k--) {
                                        idx = "[" + n % sourceShape[k] +"]" + idx;
                                        n = Math.floor(n/sourceShape[k]);
                                    }
                                    rhs = source_tmp_name + idx; 
                                }
                                else {
                                    lhs = "(" + getPointerCast(i, maxDepth, ast.typeInfo.OpenCLType) +
                                        ast.memBuffers.list[i] + ")" + "[" + j + "]";
                                    rhs = "&((" + getPointerCast(i+1, maxDepth, ast.typeInfo.OpenCLType)
                                        + ast.memBuffers.list[i+1]+ ")" + "[" + j*sourceShape[i+1] + "]" + ")";
                                }
                                s_tmp += lhs + " = " + rhs + " ,"; 
                            }
                            redu = redu*sourceShape[i];
                        }
                        s_tmp += " (" + sourceType + ")" + ast.memBuffers.list[0] + ")";
                        s += s_decl + "(" + ast.children[0].value + (ast.assignOp ? tokens[ast.assignOp] : "") + "= " + s_tmp + ")";
                    }
                    else if(ast.typeInfo.isScalarType()) {
                        // Do scalars ever have memory allocated to
                        // them ?
                        throw new Error("Compiler bug: Memory allocated for scalar copy");
                    }
                    
                } else if (ast.children[1].type == ARRAY_INIT) {
                    ast.children[1].assignedIdentifier = ast.children[0].value;
                    // do not assign
                    s = s + "(" + glslExpression(ast.children[1]) + ")"; // no ; because ASSIGN is an expression!
                } else if (ast.children[1].typeInfo.isSlangArray()) {
                    // RHS is an array?
                    console.log('skip assignment for', Narcissus.decompiler.pp(ast.children[1]));
                    ast.children[1].assignedIdentifier = ast.children[0].value;
                    // don't generate the assignment, since call returns void
                    s = s + "(" + glslExpression(ast.children[1]) + ")"; // no ; because ASSIGN is an expression!
                }  else {
                    s = s + "(" + RENAME(ast.children[0].value) + (ast.assignOp ? tokens[ast.assignOp] : "") + "= " + glslExpression(ast.children[1]) + ")"; // no ; because ASSIGN is an expression!
                }
                break;
            case INDEX:
                // array update <expr>[iv] = expr
                // make sure that <expr> is in the __private address space. We catch it this late just for
                // prototyping convenience. Could go anywhere after TI.
                // (findSelectionRoot(ast.children[0]).typeInfo.getOpenCLAddressSpace() !== "__global") || reportError("global arrays are immutable", ast);
                
                s = s + "((" + glslExpression(ast.children[0]) + ")" + (ast.assignOp ? tokens[ast.assignOp] : "") + "= " + glslExpression(ast.children[1]) + ")";
                break;
            case DOT:
                // object property update.
                // a.b = c;
                // make sure that address spaces are right!
                s = s + "((" + glslExpression(ast.children[0].children[0]) + "->" + ast.children[0].children[1].value + ")" + (ast.assignOp ? tokens[ast.assignOp] : "") + "= " + glslExpression(ast.children[1]) + ")" ;
                break;
            default:
                reportBug("unhandled lhs in assignment");
                break;
            }
            // leave the last type in the accu. Assignments can be expressions :)
            break;
            
            // 
            // expressions
            //
        case COMMA:
            for (var i=0; i<ast.children.length;i++) {
                if (i>0) {
                    s += ", ";
                }
                s += glslExpression(ast.children[i]);
            }            
            break;
        case HOOK:
            // the hook (?) is badly designed. The first child is the condition, second child
            // the then expression, third child the else expression
            s += "("+glslExpression(ast.children[0])+"?"
                +glslExpression(ast.children[1])+":"
                +glslExpression(ast.children[2])+")";
            break;
            
            // binary operations on all literals
        case STRICT_EQ:
        case STRICT_NE:
            // we map these to the no strict case for now
            ast.value = ast.value.substring(0,2);
            // fallthrough;
            
        case PLUS: 
            // we do not support strings yet, so this case is the same as numbers
            // fallthrough
            
            // binary operators on numbers (incl bool)
        case BITWISE_OR:
        case BITWISE_XOR:
        case BITWISE_AND:
        case EQ:
        case NE:
        case LT:
        case LE:
        case GE:
        case GT:
        case LSH:
        case RSH:
        case URSH:
        case MINUS:
        case MUL:
        case DIV:
            s = s + "("+glslExpression(ast.children[0]) + ast.value + glslExpression(ast.children[1]) + ")";
            break;
        case MOD: 
            s = s + "(" + "fmod(" + "(" + glslExpression(ast.children[0]) + ")" + ", " + "(" + glslExpression(ast.children[1]) + ")" + ")" + ")";
            break;
            
            // binary operators on bool
        case OR:
            s += "("+glslExpression(ast.children[0])+" || "+glslExpression(ast.children[1])+")";
            break;
        case AND:
            s += "("+glslExpression(ast.children[0])+" && "+glslExpression(ast.children[1])+")";
            break;

            // unary functions on all literals
        case NOT:
        case BITWISE_NOT:
        case UNARY_PLUS:
        case UNARY_MINUS:
            s = s + ast.value + glslExpression(ast.children[0]);
            break;
            // unary functions on numbers (incl bool)
        case INCREMENT:
        case DECREMENT:
            var incArg = stripCasts(ast.children[0]);
            var incType = ast.children[0].typeInfo.OpenCLType;
            if (ast.postfix) {
                s = s + glslExpression(ast.children[0]) + ast.value;
            } else {
                s = s + ast.value + glslExpression(ast.children[0]);
            }
            break;

            // literals
        case IDENTIFIER:
            s = s + RENAME(ast.value);
            break;
        case THIS:
            s = s + " tempThis "; // This should come from the boilerplate but that cannot be passed around easily
            break;
        case DOT:
            if (ast.children[0].typeInfo.isObjectType("Attributes")) { // KP: FIXME check??
                s = ast.children[1].value; 
            } else if (ast.children[0].typeInfo.isObjectType("Uniforms")) { // KP: FIXME check??
                s = ast.children[1].value; 
            } else if (ast.children[0].typeInfo.isObjectType("Varyings")) { // KP: FIXME check?? only valid for fragment shaders?
                s = ast.children[1].value; 
            } else if (ast.children[0].typeInfo.isObjectType("InlineObject")) {
                // TypeInference would have checked if this property selection
                // is valid
                s = s + " " + RENAME(ast.children[0].value) + "->" + ast.children[1].value;
            } else if ((ast.children[0].typeInfo.isArrayishType()) &&
                       (ast.children[1].value === "length")) {
                // length property -> substitute the value
                s = s + ast.children[0].typeInfo.getOpenCLShape()[0];
            } else {
                reportBug("unsupported property selection in back end", ast);
            }
            break;

        case CAST:
        case FLATTEN:
        case NUMBER:
            s += glslExpression(ast);
            break;
        case TRUE:
            s += "true";
            break;
        case FALSE:
            s += "false";
            break;

            // array operations
        case INDEX:
            var lhs = ast.children[0].typeInfo;
            if (lhs.isObjectType("ParallelArray")) {
                if (lhs.properties.shape.length == 1) {
                    var rank = lhs.properties.shape[0];
                    var index = Number(ast.children[1].value);
                    var succ = false;
                    switch (index) {
                    case 0:
                        if (rank >= 0) {
                            s += glslExpression(ast.children[0]) + ".x";
                            succ = true;
                        }
                        break;
                    case 1:
                        if (rank >= 1) {
                            s += glslExpression(ast.children[0]) + ".y";
                            succ = true;
                        }
                        break;
                    case 2:
                        if (rank >= 2) {
                            s += glslExpression(ast.children[0]) + ".z";
                            succ = true;
                        }
                        break;
                    case 3:
                        if (rank >= 3) {
                            s += glslExpression(ast.children[0]) + ".w";
                            succ = true;
                        }
                        break;
                    }
                    if (!succ) {
                        s += "FIXME(failed to index ParallelArray with shape " + lhs.properties.shape + " on " + ast.children[1].value + ")";
                    }

                } else {
                    s += "FIXME(some kind of ParallelArray)";
                }

            } else {
                s += compileSelectionOperation(ast, ast.children[0], ast.children[1], true);
            }
            break;

        case ARRAY_INIT:
            if (!ast.assignedIdentifier) 
                reportError("array literals supported only in variable initializers");
            s = s + "(";
            for (var i=0;i<ast.children.length;i++) {
                if (i>0) {
                    s += ", ";
                }
                s += /*ast.allocatedMem*/ RENAME(ast.assignedIdentifier) + "[" + i + "] = " + glslExpression(ast.children[i]);
            }
            /*
            if (i>0) {
                s += ", ";
            }
            s = s  + ast.allocatedMem + ")";
            */
            s = s + ")";
            //}
            break;

            // function application
        case CALL:
            s += glslCallExpression(ast);
            break;
            // Below is the typ

            // argument lists
        case LIST:      
            for (var i=0; i<ast.children.length;i++) {
                if (i>0) {
                    s += ", ";
                }
                s += glslExpression(ast.children[i]);
            }   
            break;

            // 
            // unsupported stuff here
            //
        case GETTER:
        case SETTER:
            reportError("setters/getters not yet implemented", ast);
            break;
        case TRY:
        case THROW:
            reportError("try/throw/catch/finally not yet implemented", ast);
            break;
        case BREAK:
            //s += " break; ";
            //break;
        case CONTINUE:
            //s += " continue; ";
            //break;
        case LABEL:
            reportError("break/continure/labels not yet implemented", ast);
            break;
        case YIELD:
        case GENERATOR:
            reportError("generators/yield not yet implemented", ast);
            break;
        case FOR_IN:
            reportError("for .. in loops not yet implemented", ast);
            break;
        case ARRAY_COMP:
        case COMP_TAIL:
            reportError("array comprehensions not yet implemented", ast);
            break;
        case NEW:
            reportError("general object construction not yet implemented", ast);
            break;
        case NEW_WITH_ARGS:
        case OBJECT_INIT:
            reportError("general object construction not yet implemented", ast);
            break;
        case WITH:
            reportError("general objects not yet implemented", ast);
            break;
        case LET:
        case LET_BLOCK:
            reportError("let not yet implemented", ast);
            break;
        case SWITCH:
            reportError("switch not yet implemented", ast);
            break;

            // unsupported binary functions
        case INSTANCEOF:
            reportError("instanceof not yet implemented", ast);
            break;
        case EQ:
        case NE:
            reportError("non-strict equality not yet implemented", ast);
            break;
        case IN:
            reportError("in not yet implemented", ast);
            break;

            // unsupported literals
        case NULL:
            reportError("null not yet implemented", ast);
            break;
        case REGEXP:
            reportError("regular expressions not yet implemented", ast);
            break;
        case STRING:
            reportError("strings not yet implemented", ast);
            break;

        case TOINT32:
            if (ast.typeInfo.isNumberType()) {
                // we have a scalar number, so we just emit the
                // conversion code
                s = s + "((int)" + glslExpression(ast.children[0]) + ")"; 
            } else {
                // this is some form of array or vector. We do not
                // have allocation of local temps, yet, so fail
                throw "TOINT32 applied to non scalar data structure";
            }
            break;
        case DEBUGGER:  // whatever this is...
        default:
            throw "unhandled node type in analysis: " + tokens[ast.type];
        }
        if (ast.parenthesized) {
            s += ")";
        }
        
        return s;
        
    }

    var checkBounds = false; // ??
    var checkall = false;

    var newError = function newError(msg) {
        return msg;
        /*
          if (verboseErrors) {
          errorMsgs[errorMsgs.length] = "AT " + (calledScope.inCalledScope() || "<top level>") + ": " + msg;
          }
          return errorMsgs.length; // this is one after the index on purpose!
        */
    };


    // Creates a potentially checked array index.
    // If the index is statically known to be correct, this
    // just returns the index itself (expr).
    // Otherwise an expression is created that checks whether
    // the index is in bounds and, if the index turns out to be 
    // out of bounds, returns 0. If the index is in bounds,
    // the value of expr is returned.
    //
    // The emmited code relies on a global variable
    // int _sel_idx_tmp
    // to store the intermediate result of evaluation expr.
    // This variable should be declared at the top-level of the
    // function.
    //
    // As a side effect, the global variable _FAIL is set to > 0
    // if a bounds check failed.
    function wrapIntoCheck(range, bound, expr, ast) {
        var postfix = "";
        var result = "";
        var dynCheck = false;

        if (bound === 0) {
            // we have an empty array => you cannot select from those
            // (yeah, yeah, I know, a real corner case :=D)
            throw new Error("selection from empty array encountered!");
        }

        if (checkBounds && 
            (checkall ||
             (range === undefined) ||
             (range.lb === undefined) ||
             (range.lb < 0))) {
            // emit lower bound check
            result += "(_sel_idx_tmp < 0 ? (_FAIL ? 0 : (_FAIL = " + newError("index " + expr + " smaller than zero in " + RiverTrail.Helper.wrappedPP(ast)) + ", 0)) : ";
            postfix = ")" + postfix;
            dynCheck = true;
        }

        if (checkBounds &&
            (checkall ||
             (range === undefined) ||
             (range.ub === undefined) ||
             (range.ub >= bound))) {
            // emit upper bound check
            result += "(_sel_idx_tmp >= " + bound + " ? (_FAIL ? 0: (_FAIL = " + newError("index " + expr + " greater than upper bound " + bound + " in " + RiverTrail.Helper.wrappedPP(ast)) + ", 0)) : ";
            postfix = ")" + postfix;
            dynCheck = true;
        }

        if (dynCheck) {
            result = "int(_sel_idx_tmp = " + expr + ", " + result + "_sel_idx_tmp" + postfix + ")";
        } else {
            result = "int(" + expr + ")";
        }

        return result;
    }


    //
    // Given we have a source and an arrayOfIndices. If the result is
    // a primitive type we return it. If it is an array then we generate a
    // pointer to the start of that array.
    //

    var compileSelectionOperation = function (ast, source, arrayOfIndices) {
        "use strict";

        var s = "";
        var i;
        var elemSize;
        var stride;
        var indexLen;
        var dynamicSel;
        var rangeInfo;
        // If arrayOfIndices has an inferredType of an array (dimSize > 0) then it is get([x, y...]);
        // If that is the case then elemRank will be the sourceRank - the length of the argument.
        var sourceType = source.typeInfo;
        var sourceShape = sourceType.getOpenCLShape();
        var sourceRank = sourceShape.length;
        var elemRank = ast.typeInfo.getOpenCLShape().length;
        if (sourceType.isObjectType("JSArray")) {
            // special treatment for JavaScript encoded arrays
            rangeInfo = arrayOfIndices.rangeInfo;

            s = s + glslExpression(source) ;
            s = s + "[" + wrapIntoCheck(rangeInfo, sourceShape[0], glslExpression(arrayOfIndices), ast) + "]";


            /*
            if (elemRank === 0) {
                // scalar case 
                s = s + "__JS_array_sel_S(" + glslExpression(source) + ", " + wrapIntoCheck(rangeInfo, sourceShape[0], glslExpression(arrayOfIndices), ast) + ")";
            } else {
                s = s + "__JS_array_sel_A(" + glslExpression(source) + ", " + wrapIntoCheck(rangeInfo, sourceShape[0], glslExpression(arrayOfIndices), ast) + ", " + sourceShape[1] + ", &_FAIL)";
            }
            */
        } else {
            // C style arrays
            var isParallelArray = sourceType.isObjectType("ParallelArray");
            var isGlobal = (sourceType.getOpenCLAddressSpace() === "__global");
            if (elemRank !== 0) {
                if(isParallelArray || isGlobal) {
                    // The result is a pointer to a sub dimension.
                    s = s + "( &";
                }
                else {
                    s = s + "(";
                }
            }
            elemSize = ast.typeInfo.getOpenCLShape().reduce( function (p,n) { return p*n;}, 1);
            if(isParallelArray || isGlobal) {
                s = s + " ( " + glslExpression(source) + "[0 ";
            }
            else {
                s = s + glslExpression(source) ;
            }

            stride = elemSize;

            if (arrayOfIndices.type !== LIST) {
                // we have a single scalar index from an INDEX op
                rangeInfo = arrayOfIndices.rangeInfo;
                if(isParallelArray || isGlobal) {
                    s = s + " + " + stride + " * ("+wrapIntoCheck(rangeInfo, sourceShape[0], glslExpression(arrayOfIndices), ast) + ")";
                }
                else {
                    s = s + "[" + wrapIntoCheck(rangeInfo, sourceShape[0], glslExpression(arrayOfIndices), ast) + "]";
                }
            } else {
                // this is a get
                if (arrayOfIndices.children[0] && (arrayOfIndices.children[0].type === ARRAY_INIT)) { 
                    // We might have get([0,0]); instead of get(0,0);
                    arrayOfIndices = arrayOfIndices.children[0];
                }
                rangeInfo = arrayOfIndices.rangeInfo;
                // the first argument could be an index vector, in which case we have to produce dynamic
                // selection code
                dynamicSel = arrayOfIndices.children[0].typeInfo.getOpenCLShape().length !== 0;
                for (i = sourceRank - elemRank - 1; i >= 0; i--) { // Ususally only 2 or 3 dimensions so ignore complexity
                    s = s + " + " + stride + " * ("
                    if (dynamicSel) {
                        s = s + wrapIntoCheck((rangeInfo ? rangeInfo.get(0).get(i) : undefined), sourceShape[i], glslExpression(arrayOfIndices.children[0], ast) + "[" + i + "]") + ")";
                    } else {
                        s = s + wrapIntoCheck((rangeInfo ? rangeInfo.get(i) : undefined), sourceShape[i], glslExpression(arrayOfIndices.children[i]), ast) + ")";
                    }
                    stride = stride * sourceType.getOpenCLShape()[i];
                }
            }
            if(isParallelArray || isGlobal) {
                s = s + "])";
            }

            if (elemRank !== 0) {
                // The result is a pointer to a sub dimension.
                s = s + " )";
            }
        }

        return s;
    }


    function emitDeclarations(env, renamer) {
        s = "";
        for (var name in env.bindings) {
            var type = env.bindings[name].type;
            // only declare variables that are actually used (and thus have a type) 
            if (type && !type.isPseudoType()) {
                s = s + " ";
                if (env.bindings[name].constinit) {
                    s += "const "; // FIXME restrict const expression initializers to allow reordering? currently they may use variables whose
                    // value computation might have been reordered, due to the fact that const initializers are emitted in GLSL at 
                    // declaration location, while regular variables are not.
                    s += type.slangTypedName((renamer ? renamer(name) : name)) 
                        + " = " + glslExpression(env.bindings[name].constinit) + ";\n";
                } else {
                    s += type.slangTypedName((renamer ? renamer(name) : name)) + ";\n";
                }
            }
        }
        return s;
    };


    function glslStatements(statements, extra) {
        //"use strict"; // seems to trigger a timing-dependent behavior in Chrome
        var i;
        var x;
        var s = "";
        if ((statements.type === BLOCK) || (statements.type === SCRIPT)) {
            if (statements.symbols) {
                s = s + emitDeclarations(statements.symbols, RENAME);
            }
            if (statements.memVars) {
                s = s + statements.memVars.declare();
            }

            for (i=0; i<statements.children.length;i++) {
                s = s + glslStatement(statements.children[i]) + ";\n";
            }
        } else {
            s = s + glslStatement(statements);
        }
        extra = extra || "";
        return "{" + s + " " + extra + "}";
    }


    var toCNumber = function (val, type) {
        var res = "";
        if ((type.OpenCLType === "float") || (type.OpenCLType === "double")) {

            res = val; 
            if ((String.prototype.indexOf.call(res, '.') === -1) && (String.prototype.indexOf.call(res, 'e') === -1)) {
                res += ".0";
            }
        } else if (type.OpenCLType === "int"){
            res = val;
        } else if (type.OpenCLType === "boolean"){
            res =  val; // CR check that this works.
        } else {
            reportBug("unexpected number value in toCNumber");
        }
        return res;
    };



    //
    // mathOCLMethod translate a javascript method in the Math class into the
    // corresponding OpenCL primitive.
    // The translation is dependent on type but defaults to float.
    //
    // For an overview of JavaScript Math methods see
    //
    // https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Math#Methods
    //

    function mathGlslMethod(ast) {
        "use strict";
        var jsMethod = (ast.children[0].children[1].value).toLowerCase();
        if (jsMethod === "abs") { // There is an int abs so we probable should check type here.
            if ((ast.children[1].typeInfo[0].OpenCLType !== "float") &&
                    (ast.children[1].typeInfo[0].OpenCLType !== "double")) {
                return "((" + ast.children[1].typeInfo[0].OpenCLType + ") fabs((float) " + glslExpression(ast.children[1]) + "))";
            } else {
                return "fabs(" + glslExpression(ast.children[1]) + ")";
            }
        }
        if (jsMethod === "round") {
            return "floor(.5 + " + glslExpression(ast.children[1]) + ")";
        }
        if (jsMethod === "atan2") {
            return "atan2(" + glslExpression(ast.children[1]) + "," + glslExpression(ast.children[1]) + ")";
        }
        if (jsMethod === "random") { // JS wants something between 0 and 1.
            return "(rand()/(RAND_MAX-1))"; // -1 so that we can never get 1 which JS random nevers returns.
        }
        if (jsMethod === "min" || jsMethod === "max" || jsMethod === "mod") { // FIXME there's no Math.mod in JS
            return jsMethod + "(" + glslExpression(ast.children[1]) + ")";
            //throw new Error("Math." + jsMethod + "Not supported"); // Not sure what the translation should be so punt back to JS.
        }
        // In all other cases the c string is the same as the JS string.
        return jsMethod + "(" + glslExpression(ast.children[1]) + ")";
    };


    function glslInfixBinOp(str) {
        switch (str) {
        case "multiply": return "*";
        case "add": return "+"; 
        case "subtract": return "-"; 
        case "divide": return "/"; 
        default: return undefined;
        }
    }

    function glslCallExpression(ast) 
    {
        //"use strict"; // seems to trigger a timing-dependent behavior in Chrome
        var s = " ";
        function lvaluep(acc) {
            return acc && (acc.type === IDENTIFIER || acc.type === INDEX);
        }

        function purep(acc) { // conservative
            return false;
        }
        function glslUnaryOp(ast) {
            // unary op with accumulator
            var rhs = ast.children[0].children[1]; // the method call
            var op = rhs.value === "negate" ? "-" : "normalize";
            var acc = ast.children[1].children[1];
            var m0 = glslExpression(ast.children[1].children[0]);
            var s = "";
            if (lvaluep(acc)) {
                s += "(" + glslExpression(acc) + " =  " + op + "(" + m0 + "))";
            } else if (ast.assignedIdentifier || ast.allocatedMem) {
                var accname = (ast.assignedIdentifier && RENAME(ast.assignedIdentifier)) || ast.allocatedMem; // return value is the same as the last argument
                if (!purep(ast))
                    console.warn("elided evaling", glslExpression(acc), "although maybe it had side effects");
                s += "(" + /*accname + " = " + glslExpression(acc) + ", "*/ accname + " = " + op + "(" + m0  + "))";
            } else reportError("cannot generate nested expression", ast); // FIXME need a temporary?
            return s;
        }
        
        function glslBinOp(ast, prefixp) {
            // binary op with accumulator
            var rhs = ast.children[0].children[1]; // the method call
            var m0 = glslExpression(ast.children[1].children[0]);
            var m1 = glslExpression(ast.children[1].children[1]);
            var acc = ast.children[1].children[2];
            var op = glslInfixBinOp(rhs.value) || rhs.value;
            var s = "";
            if (lvaluep(acc)) {
                s += "(" + glslExpression(acc) + ' = ';
                if (!prefixp)
                    s += m0 + op + m1;
                else
                    s += op + "(" + m0 + "," + m1 + ")";
                s += ")";
            } else if (ast.assignedIdentifier || ast.allocatedMem) {
                var accname = (ast.assignedIdentifier && RENAME(ast.assignedIdentifier)) || ast.allocatedMem; // return value is the same as the last argument
                // FIXME incomprehensiby, evaluating comma expressions doesn't work as expected ( "(vec(3), x + y)" doesn't seem to do the right thing.
                if (!purep(ast))
                    console.warn("elided evaling", glslExpression(acc), "although maybe it had side effects");
                s += "(" + accname + " = ";
                if (!prefixp)
                    s += m0 + op + m1;
                else
                    s += op + "(" + m0 + "," + m1 + ")";
                s += ")";
            } else reportError("cannot generate nested expression", ast); // FIXME, should not happen
            return s;
        }

        function binopp(ast) {
            return (ast.value == "multiply" || ast.value == "subtract" || ast.value == "add" || ast.value == "divide" || ast.value == "reflect")
        }

        // Deal with some intrinsics if found, otherwise just make the call.
        if (ast.children[0].type === DOT ) {
            var lhs = ast.children[0].children[0]; // the object
            var rhs = ast.children[0].children[1]; // the method call
            if (lhs.value === "Math") {
                s = s + mathGlslMethod(ast);
            } else if (isGlMatrixVecSingleton(lhs.value)) {
                if (rhs.value === "createFrom" || rhs.value === "create") {
                    var actuals = glslExpression(ast.children[1]);
                    s +=  lhs.value + "(" + ((rhs.value == "create" && ast.children[1].children.length == 0) ? "0.0" : actuals) + ")"; // FIXME the value should be type-specific
                } else if (binopp(rhs)) {
                    s += glslBinOp(ast, rhs.value === "reflect");
                } else if (rhs.value === "negate" || rhs.value === "normalize") {
                    s += glslUnaryOp(ast);
                }  else if (rhs.value === "dot") {
                    // binary without accumulator
                    var m0 = glslExpression(ast.children[1].children[0]);
                    var m1 = glslExpression(ast.children[1].children[1]);
                    s += rhs.value + "(" + m0 + "," + m1  +")";
                } else
                    s = s + " 628 glslExpression not complete probable some sort of method call: " + Narcissus.decompiler.pp(ast);
            } else if (isGlMatrixMatSingleton(lhs.value)) {
                if (rhs.value === "createFrom" || rhs.value === "create") {
                    var actuals = glslExpression(ast.children[1]);
                    s +=  lhs.value + "(" + actuals + ")"; // FIXME this should be type-specific
                } else if (binopp(rhs)) {
                    s += glslBinOp(ast, rhs.value === "reflect");
                } else  {
                    s = s + " 628 glslExpression not complete probable some sort of method call: " + Narcissus.decompiler.pp(ast);
                }
            } else if (lhs.value == "texture2D") {
                if (rhs.value == "create") {
                    var actuals = glslExpression(ast.children[1]);
                    s += "texture2D(" + actuals + ")";
                } else {
                    s = s + " 628 glslExpression not complete probable some sort of method call: " + Narcissus.decompiler.pp(ast);
                }
            } else {
                debugger;
                s = s + " 628 glslExpression not complete probable some sort of method call: " + Narcissus.decompiler.pp(ast);
            }
        } else { // It is not a method call.
                var actuals = "";
                actuals = glslExpression(ast.children[1]);
                var post_parens = "";
                if(ast.typeInfo.name === "InlineObject") {
                    //var rootBuffer = ast.memBuffers.__root;
                    var root_index = 0;
                    //s += "(";
                    for(var idx in ast.typeInfo.properties.fields) {
                        var propType = ast.typeInfo.properties.fields[idx];
                        if(propType.isScalarType())
                            continue;
                        // If this property is an array, create the nesting
                        // structure if needed
                        var propShape = propType.getOpenCLShape();
                        var maxDepth = propShape.length;
                        var post_parens = "";
                        var redu = 1; var rhs = ""; var lhs = ""; post_parens = ")";
                        /*
                        for(var i = 0 ; i < maxDepth-1; i++) {
                            for(var j = 0; j < propShape[i]*redu; j++) {
                                lhs = "(" + getPointerCast(i, maxDepth, propType.OpenCLType) +
                                    ast.memBuffers[idx][i] + ")"
                                    + "[" + j + "]";
                                rhs = "&((" + getPointerCast(i+1, maxDepth, propType.OpenCLType)
                                    + ast.memBuffers[idx][i+1]
                                    + ")" + "[" + j*propShape[i+1] + "]" + ")";
                                s += lhs + " = " + rhs + " ,";
                            }
                            redu = redu*propShape[i];
                        }
                        s += "((" + ast.typeInfo.OpenCLType + ")" + rootBuffer + ")" + "->" + idx + "=" +
                            "(" + propType.OpenCLType + ")" + ast.memBuffers[idx][0] + ",";
                        */
                    }
                    //s = s + RENAME(ast.children[0].dispatch) + "(" (actuals !== "" ? ", " : "") + actuals;
                    // s += ", (" + ast.typeInfo.OpenCLType + ")" + rootBuffer + "))";
                }
                else if(!(ast.typeInfo.isScalarType()) && ast.typeInfo.getOpenCLShape().length > 1) {
                    // Create structure if this call is going to return a nested
                    // array
                    console.log(ast.children[0], ast.children[1]);
                    s += "("; // + "/*(initNestedArrayStructure(ast, false)*/)"
                    post_parens = ")";
                    // NOTE: use renamed dispatch name here!
                    s = s + RENAME(ast.children[0].dispatch) + "("  + actuals;

                    s = s + ", " + RENAME(ast.assignedIdentifier);
                        //s = s + ", (" + ast.typeInfo.OpenCLType + ") " + ast.allocatedMem;
                    s = s + ")";
                    s = s + post_parens; // Close the comma list.
                }
                else {
                    if (ast.children[0].value === "Number") {
                        // the only builtin function handled.
                        s = s + "float(" + actuals;
                        // FIXME make sure redefining Number (and other intrinsics) is a no-no
                    } else {
                    // NOTE: use renamed dispatch name here!
                        s = s + RENAME(ast.children[0].dispatch) + "(" +  actuals;
                    }
                    /*
                    if (!(ast.typeInfo.isScalarType())) {
                        s = s + ", (" + ast.typeInfo.OpenCLType + ") " + ast.allocatedMem;
                    }
                    */
                    s = s + ")";
                    s = s + post_parens;
               }
        }
        return s;
    };


    function glslExpression(ast) {
        //"use strict"; // seems to trigger a timing-dependent behavior in Chrome
        var s = " ";
        var i, ii;
        var arrayOfIndices;
        var stride;
        var elemSize;
        var elemRank;
        var sourceRank;
        var indexLen;
        if (ast.type === CAST) {  // deals with adding things like (float *)mumble) to generated code. 
            if (!ast.typeInfo.isScalarType()) {
                reportError("non-scalar cast encountered", ast);
            }
            s = "((" + ast.typeInfo.OpenCLType + ")" 
                + glslExpression(ast.children[0]) + ")";
            //}
        } else if (ast.type === FLATTEN) {
            if (ast.typeInfo.getOpenCLShape().length > 1) {
                reportError("flattening of nested arrays not yet implemented.", ast.children[0]);
            }
            s = s + glslExpression(ast.children[0]);
        } else if (ast.type === NUMBER) {
            s = s + toCNumber(ast.value, ast.typeInfo);

        } else if (ast.type === THIS) {
            s = s + " tempThis "; // SAH: this should come from the boilerplate but that cannot be passed around easily

        } else if (ast.type === CALL) {
            return glslCallExpression(ast);
            // Deal with some intrinsics if found, otherwise just make the call.
        } else {
            // Everything else can be dealt with according to the more straight forward translation.
            s = glslStatement(ast);
        }
        return s;
    }


    // If you are working inside the top level of actual kernel function then scope is empty.
    // If you generating code for a called function then this will be true.
    var calledScope = function () {
        "use strict";
        // state is private.
        var state = false;
        var enter = function enter(name) {
            state = name || true;
        };
        var exit = function exit(previous) {
            state = previous;
        }
        var inCalledScope = function () {
            return state;
        };
        return {"enter": enter, "exit": exit, "inCalledScope": inCalledScope};
    } ();

    
    function genFormalParams(formalsAst, construct) {
        "use strict";
        if (calledScope.inCalledScope()) {
            return genNonKernelFormalParams(formalsAst);
        } else {
            return genKernelFormalParams(formalsAst, construct);
        }
    }
    // This is for called functions, not the top level kernel function.
    function genNonKernelFormalParams(formalsAst) {
        "use strict";
        var i;
        var s = "";
        var formalsNames = formalsAst.params;
        var formalsTypes = formalsAst.typeInfo.parameters;

        for (i = 0; i < formalsTypes.length; i++) {
            if (s !== "" ) { 
                s = s + ", "; // leave out the , before the first parameter
            }

            /*
            if (formalsTypes[i].isArrayishType()) {
                // array argument, so needs address space qualifier
                s = s + formalsTypes[i].getOpenCLAddressSpace() + " ";
            }
            */
            s = s + formalsTypes[i].slangTypedName(RENAME(formalsNames[i]));
        }
        return s;
    };

    function genKernelFormalParams(formalsAst, construct) {
        "use strict";
        var i;
        var s = "";
        var formalsNames = formalsAst.params;
        var formalsTypes = formalsAst.typeInfo.parameters;
        if (construct === "combine") { 
            // Skip the extra type for this and ignore the first argument.
            // the extras do not include |this| and the first formal since that is the index generated in the body.

            formalsNames = formalsNames.slice(1); // This skips the index argument
            formalsTypes = formalsTypes.slice(2); // This skips this and the index argument
        } else if ((construct === "comprehension") || (construct === "comprehensionScalar")) {
            // ignore the first argument, the index
            formalsTypes = formalsTypes.slice(1);
            formalsNames = formalsNames.slice(1);
        } else if (construct === "map") {
            // Skip the extra type for this
            // Skip the extra type for this and ignore the first argument, which is the value and is set
            // explicitly based on this and the id.
            // the extras do not include |this| and the first formal since that is the value generated in the body.
            formalsTypes = formalsTypes.slice(2); // Skip this and the val argument.
            formalsNames = formalsNames.slice(1); // Skip the val argument
        }

        for (i = 0; i < formalsTypes.length; i++) {
            if (s !== "" ) { 
                s = s + ", "; // leave out the , before the first parameter
            }
            
            s = s + formalsTypes[i].slangTypedName(RENAME(formalsNames[i]));
        }
        return s;
    }

    //
    // This generates code for a function that is presmable called from the kernel function.
    // 
    //
    function genCalledFunctionHeader(ast) {
        "use strict";
        var s = "";
        var formals = "";
        
        if (ast.value != "function") {
            throw "expecting function found " + ast.value;
        }
        
        var previousCalledScope = calledScope.inCalledScope();
        calledScope.enter(ast.name);
        var arrayReturn = ast.typeInfo.result.isSlangArray();
        // Need code here to deal with array values being returned.
        // NOTE: use dispatched function name here

        s = s + " " + (arrayReturn ? "void" : ast.typeInfo.result.OpenCLType) + " " + RENAME(ast.dispatch);
        s = s + "("; // start param list.
        // add extra parameter for failure propagation
        formals = genFormalParams(ast, "ignore");
        if (formals !== "") {
            s = s + formals;
        } // else there are no formals to output.
        
        var returnType = ast.typeInfo.result.OpenCLType;
        
        if (arrayReturn) { // This assumes it is an array.
            // If the return type of the result is an Array then a pointer to it is passed in.
            //s = s + ", " + returnType + " retVal";
            s += ", " + ast.typeInfo.result.slangTypedName("_retVal"); // FIXME renaming
        }

        s = s + " )";
        
        calledScope.exit(previousCalledScope);
        
        return s;
    }
    
    function genCalledFunction(ast) {
        "use strict";
        var s = "";
        
        var previousCalledScope = calledScope.inCalledScope();
        calledScope.enter(ast.name);
        if (ast.value != "function") {
            throw "expecting function found " + ast.value;
        }
        var returnType = ast.typeInfo.result.OpenCLType;
        s = s + genCalledFunctionHeader(ast);
        //s = s + " { ";// function body
        //s = s + returnType + " " + "tempResult" + ";"; // tmp var for parking result
        s = s + glslStatements(ast.body); // Generate the statements;
        // s = s + " }";
        s = s + "\n";
        
        calledScope.exit(previousCalledScope);
        return s;
    }
    
    function declareLocalFunctions(decls) {
        var s = "";
        
        if (!decls)
            return s;
        
        for (var cnt = 0; cnt < decls.length; cnt ++) {
            if (decls[cnt].isBuiltin) {
                continue;
            }
            s = s + declareLocalFunctions(decls[cnt].body.funDecls);
            s = s + genCalledFunctionHeader(decls[cnt]) + ";\n";
        }
        
        return s;
    }
    
    function generateLocalFunctions(decls) {
        var s = "";
        
        if (!decls)
            return s;
        
        for (var cnt = 0; cnt < decls.length; cnt ++) {
            if (decls[cnt].isBuiltin) {
                continue;
            }
            s = s + generateLocalFunctions(decls[cnt].body.funDecls);
            s = s + genCalledFunction(decls[cnt]);
        }
        
        return s;
    }

    function compileVertexShader(args) {
        //var src = document.getElementById(func.glslElement).innerHTML;
        //console.log('/*orig*/ ' + src);
        var comp = "precision mediump float;\n";
        var ast = RiverTrail.Helper.parseFunction(args.vertexShader);
        var paSource = undefined;
        var construct = undefined;
        var rank = undefined;
        var lowPrecision = undefined;
        
        ShaderDSL.Typeinference.analyze(ast, paSource, construct, args.attributes, args.uniforms, undefined, lowPrecision);
        ShaderDSL.InferMem.infer(ast);

        var body = glslStatements(ast.body);

        comp += emitUniformDecls(args);
        comp += emitAttributeDecls(args);
        comp += emitVaryings();
        
        comp += declareLocalFunctions(ast.body.funDecls);
        comp += generateLocalFunctions(ast.body.funDecls);

        comp += "void main()\n" + body;
        console.log('/*xltd*/ ' + comp);
        return comp;
    }

    function compileFragmentShader(args) {
        //var src = document.getElementById(func.glslElement).innerHTML;
        //console.log('/*orig*/ ' + src);
        var ast = RiverTrail.Helper.parseFunction(args.fragmentShader)
        var paSource = undefined;
        var construct = undefined;
        var rank = undefined;
        var lowPrecision = undefined;

        ShaderDSL.Typeinference.analyze(ast, paSource, construct, undefined, args.uniforms, varyingTypes, lowPrecision);
        ShaderDSL.InferMem.infer(ast);

        var body = glslStatements(ast.body);

        var comp = "precision mediump float;\n";
        comp += emitUniformDecls(args); // FIXME same uniforms as vertex shader? or only referenced uniforms? or?
        comp += emitVaryings();

        comp += declareLocalFunctions(ast.body.funDecls);
        comp += generateLocalFunctions(ast.body.funDecls);

        comp += "void main()\n" + body;
        console.log('/*xltd*/ ' + comp);
        return comp;
    }

    function compileGladderProgram(args) {
        function stripArrays(decls)  { 
            // probably wrong to make gladder believe that arrays are scalars.
            var result = {};
            for (var k in decls) {
                if (!decls.hasOwnProperty(k)) continue;
                if ((k.indexOf("vec") == 0 || k.indexOf("mat") == 0) && k.indexOf("[") >= 0) {
                    // FIXME that's a simplification
                    k = k.substring(0, 4); 
                }
                result[k] = decls[k];
            }
            return result;
        }
        return { 
            vertexShader: ShaderDSL.compileVertexShader(args),
            fragmentShader: ShaderDSL.compileFragmentShader(args),
            attributes: stripArrays(args.attributes),
            uniforms: stripArrays(args.uniforms)
        };
    }

    window.ShaderDSL = {
        compileVertexShader: compileVertexShader,
        compileFragmentShader: compileFragmentShader,
        compileGladderProgram: compileGladderProgram,
        parseGladderArrayDecl: parseGladderArrayDecl,
        isGlMatrixSingleton: isGlMatrixSingleton,
        isGlMatrixMatSingleton : isGlMatrixMatSingleton
    };

})();