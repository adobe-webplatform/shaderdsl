/**  
  Copyright (c) 2013 Adobe Systems Incorporated. All rights reserved.
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/


/* vim: set sw=4 ts=4 et tw=78: */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is the Narcissus JavaScript engine.
 *
 * The Initial Developer of the Original Code is
 * Brendan Eich <brendan@mozilla.org>.
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Tom Austin <taustin@ucsc.edu>
 *   Brendan Eich <brendan@mozilla.org>
 *   Shu-Yu Guo <shu@rfrn.org>
 *   Dave Herman <dherman@mozilla.com>
 *   Dimitris Vardoulakis <dimvar@ccs.neu.edu>
 *   Patrick Walton <pcwalton@mozilla.com>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

/*
 * Narcissus - JS implemented in JS.
 *
 * Well-known constants and lookup tables.  Many consts are generated from the
 * tokens table via eval to minimize redundancy, so consumers must be compiled
 * separately to take advantage of the simple switch-case constant propagation
 * done by SpiderMonkey.
 */

(function() {

    var narcissus = {
        options: {
            version: 185,
            // Global variables to hide from the interpreter
            hiddenHostGlobals: { Narcissus: true },
            // Desugar SpiderMonkey language extensions?
            desugarExtensions: false,
            // Allow HTML comments?
            allowHTMLComments: false
        },
        hostSupportsEvalConst: (function() {
            try {
                return eval("(function(s) { eval(s); return x })('const x = true;')");
            } catch (e) {
                return false;
            }
        })(),
        hostGlobal: this
    };
    Narcissus = narcissus;
})();

Narcissus.definitions = (function(hostGlobal) {

    var tokens = [
        // End of source.
        "END",

        // Operators and punctuators.  Some pair-wise order matters, e.g. (+, -)
        // and (UNARY_PLUS, UNARY_MINUS).
        "\n", ";",
        ",",
        "=",
        "?", ":", "CONDITIONAL",
        "||",
        "&&",
        "|",
        "^",
        "&",
        "==", "!=", "===", "!==",
        "<", "<=", ">=", ">",
        "<<", ">>", ">>>",
        "+", "-",
        "*", "/", "%",
        "!", "~", "UNARY_PLUS", "UNARY_MINUS",
        "++", "--",
        ".",
        "[", "]",
        "{", "}",
        "(", ")",

        // Nonterminal tree node type codes.
        "SCRIPT", "BLOCK", "LABEL", "FOR_IN", "CALL", "NEW_WITH_ARGS", "INDEX",
        "ARRAY_INIT", "OBJECT_INIT", "PROPERTY_INIT", "GETTER", "SETTER",
        "GROUP", "LIST", "LET_BLOCK", "ARRAY_COMP", "GENERATOR", "COMP_TAIL",

        // Terminals.
        "IDENTIFIER", "NUMBER", "STRING", "REGEXP",

        // Keywords.
        "break",
        "case", "catch", "const", "continue",
        "debugger", "default", "delete", "do",
        "else", "export",
        "false", "finally", "for", "function",
        "if", "import", "in", "instanceof",
        "let", "module",
        "new", "null",
        "return",
        "switch",
        "this", "throw", "true", "try", "typeof",
        "var", "void",
        "yield",
        "while", "with",
    ];

    var statementStartTokens = [
        "break",
        "const", "continue",
        "debugger", "do",
        "for",
        "if",
        "return",
        "switch",
        "throw", "try",
        "var",
        "yield",
        "while", "with",
    ];

    // Whitespace characters (see ECMA-262 7.2)
    var whitespaceChars = [
        // normal whitespace:
        "\u0009", "\u000B", "\u000C", "\u0020", "\u00A0", "\uFEFF",

        // high-Unicode whitespace:
        "\u1680", "\u180E",
        "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005", "\u2006",
        "\u2007", "\u2008", "\u2009", "\u200A",
        "\u202F", "\u205F", "\u3000"
    ];

    var whitespace = {};
    for (var i = 0; i < whitespaceChars.length; i++) {
        whitespace[whitespaceChars[i]] = true;
    }

    // Operator and punctuator mapping from token to tree node type name.
    // NB: because the lexer doesn't backtrack, all token prefixes must themselves
    // be valid tokens (e.g. !== is acceptable because its prefixes are the valid
    // tokens != and !).
    var opTypeNames = {
        '\n':   "NEWLINE",
        ';':    "SEMICOLON",
        ',':    "COMMA",
        '?':    "HOOK",
        ':':    "COLON",
        '||':   "OR",
        '&&':   "AND",
        '|':    "BITWISE_OR",
        '^':    "BITWISE_XOR",
        '&':    "BITWISE_AND",
        '===':  "STRICT_EQ",
        '==':   "EQ",
        '=':    "ASSIGN",
        '!==':  "STRICT_NE",
        '!=':   "NE",
        '<<':   "LSH",
        '<=':   "LE",
        '<':    "LT",
        '>>>':  "URSH",
        '>>':   "RSH",
        '>=':   "GE",
        '>':    "GT",
        '++':   "INCREMENT",
        '--':   "DECREMENT",
        '+':    "PLUS",
        '-':    "MINUS",
        '*':    "MUL",
        '/':    "DIV",
        '%':    "MOD",
        '!':    "NOT",
        '~':    "BITWISE_NOT",
        '.':    "DOT",
        '[':    "LEFT_BRACKET",
        ']':    "RIGHT_BRACKET",
        '{':    "LEFT_CURLY",
        '}':    "RIGHT_CURLY",
        '(':    "LEFT_PAREN",
        ')':    "RIGHT_PAREN"
    };

    // Hash of keyword identifier to tokens index.  NB: we must null __proto__ to
    // avoid toString, etc. namespace pollution.
    var keywords = {__proto__: null};

    // Define const END, etc., based on the token names.  Also map name to index.
    var tokenIds = {};

    // Building up a string to be eval'd in different contexts.
    var consts = Narcissus.hostSupportsEvalConst ? "const " : "var ";
    for (var i = 0, j = tokens.length; i < j; i++) {
        if (i > 0)
            consts += ", ";
        var t = tokens[i];
        var name;
        if (/^[a-z]/.test(t)) {
            name = t.toUpperCase();
            keywords[t] = i;
        } else {
            name = (/^\W/.test(t) ? opTypeNames[t] : t);
        }
        consts += name + " = " + i;
        tokenIds[name] = i;
        tokens[t] = i;
    }
    consts += ";";

    var isStatementStartCode = {__proto__: null};
    for (i = 0, j = statementStartTokens.length; i < j; i++)
        isStatementStartCode[keywords[statementStartTokens[i]]] = true;

    // Map assignment operators to their indexes in the tokens array.
    var assignOps = ['|', '^', '&', '<<', '>>', '>>>', '+', '-', '*', '/', '%'];

    for (i = 0, j = assignOps.length; i < j; i++) {
        t = assignOps[i];
        assignOps[t] = tokens[t];
    }

    function defineGetter(obj, prop, fn, dontDelete, dontEnum) {
        Object.defineProperty(obj, prop,
                              { get: fn, configurable: !dontDelete, enumerable: !dontEnum });
    }

    function defineGetterSetter(obj, prop, getter, setter, dontDelete, dontEnum) {
        Object.defineProperty(obj, prop, {
            get: getter,
            set: setter,
            configurable: !dontDelete,
            enumerable: !dontEnum
        });
    }

    function defineMemoGetter(obj, prop, fn, dontDelete, dontEnum) {
        Object.defineProperty(obj, prop, {
            get: function() {
                var val = fn();
                defineProperty(obj, prop, val, dontDelete, true, dontEnum);
                return val;
            },
            configurable: true,
            enumerable: !dontEnum
        });
    }

    function defineProperty(obj, prop, val, dontDelete, readOnly, dontEnum) {
        Object.defineProperty(obj, prop,
                              { value: val, writable: !readOnly, configurable: !dontDelete,
                                enumerable: !dontEnum });
    }

    // Returns true if fn is a native function.  (Note: SpiderMonkey specific.)
    function isNativeCode(fn) {
        // Relies on the toString method to identify native code.
        return ((typeof fn) === "function") && fn.toString().match(/\[native code\]/);
    }

    var Fpapply = Function.prototype.apply;

    function apply(f, o, a) {
        return Fpapply.call(f, [o].concat(a));
    }

    var applyNew;

    // ES5's bind is a simpler way to implement applyNew
    if (Function.prototype.bind) {
        applyNew = function applyNew(f, a) {
            return new (f.bind.apply(f, [,].concat(a)))();
        };
    } else {
        applyNew = function applyNew(f, a) {
            switch (a.length) {
              case 0:
                return new f();
              case 1:
                return new f(a[0]);
              case 2:
                return new f(a[0], a[1]);
              case 3:
                return new f(a[0], a[1], a[2]);
              default:
                var argStr = "a[0]";
                for (var i = 1, n = a.length; i < n; i++)
                    argStr += ",a[" + i + "]";
                return eval("new f(" + argStr + ")");
            }
        };
    }

    function getPropertyDescriptor(obj, name) {
        while (obj) {
            if (({}).hasOwnProperty.call(obj, name))
                return Object.getOwnPropertyDescriptor(obj, name);
            obj = Object.getPrototypeOf(obj);
        }
    }

    function getPropertyNames(obj) {
        var table = Object.create(null, {});
        while (obj) {
            var names = Object.getOwnPropertyNames(obj);
            for (var i = 0, n = names.length; i < n; i++)
                table[names[i]] = true;
            obj = Object.getPrototypeOf(obj);
        }
        return Object.keys(table);
    }

    function getOwnProperties(obj) {
        var map = {};
        for (var name in Object.getOwnPropertyNames(obj))
            map[name] = Object.getOwnPropertyDescriptor(obj, name);
        return map;
    }

    function blacklistHandler(target, blacklist) {
        var mask = Object.create(null, {});
        var redirect = Dict.create(blacklist).mapObject(function(name) { return mask; });
        return mixinHandler(redirect, target);
    }

    function whitelistHandler(target, whitelist) {
        var catchall = Object.create(null, {});
        var redirect = Dict.create(whitelist).mapObject(function(name) { return target; });
        return mixinHandler(redirect, catchall);
    }

    function mirrorHandler(target, writable) {
        var handler = makePassthruHandler(target);

        var defineProperty = handler.defineProperty;
        handler.defineProperty = function(name, desc) {
            if (!desc.enumerable)
                throw new Error("mirror property must be enumerable");
            if (!desc.configurable)
                throw new Error("mirror property must be configurable");
            if (desc.writable !== writable)
                throw new Error("mirror property must " + (writable ? "" : "not ") + "be writable");
            defineProperty(name, desc);
        };

        handler.fix = function() { };
        handler.getOwnPropertyDescriptor = handler.getPropertyDescriptor;
        handler.getOwnPropertyNames = getPropertyNames.bind(handler, target);
        handler.keys = handler.enumerate;
        handler["delete"] = function() { return false; };
        handler.hasOwn = handler.has;
        return handler;
    }

    /*
     * Mixin proxies break the single-inheritance model of prototypes, so
     * the handler treats all properties as own-properties:
     *
     *                  X
     *                  |
     *     +------------+------------+
     *     |                 O       |
     *     |                 |       |
     *     |  O         O    O       |
     *     |  |         |    |       |
     *     |  O    O    O    O       |
     *     |  |    |    |    |       |
     *     |  O    O    O    O    O  |
     *     |  |    |    |    |    |  |
     *     +-(*)--(w)--(x)--(y)--(z)-+
     */

    function mixinHandler(redirect, catchall) {
        function targetFor(name) {
            return hasOwn(redirect, name) ? redirect[name] : catchall;
        }

        function getMuxPropertyDescriptor(name) {
            var desc = getPropertyDescriptor(targetFor(name), name);
            if (desc)
                desc.configurable = true;
            return desc;
        }

        function getMuxPropertyNames() {
            var names1 = Object.getOwnPropertyNames(redirect).filter(function(name) {
                return name in redirect[name];
            });
            var names2 = getPropertyNames(catchall).filter(function(name) {
                return !hasOwn(redirect, name);
            });
            return names1.concat(names2);
        }

        function enumerateMux() {
            var result = Object.getOwnPropertyNames(redirect).filter(function(name) {
                return name in redirect[name];
            });
            for (name in catchall) {
                if (!hasOwn(redirect, name))
                    result.push(name);
            };
            return result;
        }

        function hasMux(name) {
            return name in targetFor(name);
        }

        return {
            getOwnPropertyDescriptor: getMuxPropertyDescriptor,
            getPropertyDescriptor: getMuxPropertyDescriptor,
            getOwnPropertyNames: getMuxPropertyNames,
            defineProperty: function(name, desc) {
                Object.defineProperty(targetFor(name), name, desc);
            },
            "delete": function(name) {
                var target = targetFor(name);
                return delete target[name];
            },
            // FIXME: ha ha ha
            fix: function() { },
            has: hasMux,
            hasOwn: hasMux,
            get: function(receiver, name) {
                var target = targetFor(name);
                return target[name];
            },
            set: function(receiver, name, val) {
                var target = targetFor(name);
                target[name] = val;
                return true;
            },
            enumerate: enumerateMux,
            keys: enumerateMux
        };
    }

    function makePassthruHandler(obj) {
        // Handler copied from
        // http://wiki.ecmascript.org/doku.php?id=harmony:proxies&s=proxy%20object#examplea_no-op_forwarding_proxy
        return {
            getOwnPropertyDescriptor: function(name) {
                var desc = Object.getOwnPropertyDescriptor(obj, name);

                // a trapping proxy's properties must always be configurable
                desc.configurable = true;
                return desc;
            },
            getPropertyDescriptor: function(name) {
                var desc = getPropertyDescriptor(obj, name);

                // a trapping proxy's properties must always be configurable
                desc.configurable = true;
                return desc;
            },
            getOwnPropertyNames: function() {
                return Object.getOwnPropertyNames(obj);
            },
            defineProperty: function(name, desc) {
                Object.defineProperty(obj, name, desc);
            },
            "delete": function(name) { return delete obj[name]; },
            fix: function() {
                if (Object.isFrozen(obj)) {
                    return getOwnProperties(obj);
                }

                // As long as obj is not frozen, the proxy won't allow itself to be fixed.
                return undefined; // will cause a TypeError to be thrown
            },

            has: function(name) { return name in obj; },
            hasOwn: function(name) { return ({}).hasOwnProperty.call(obj, name); },
            get: function(receiver, name) { return obj[name]; },

            // bad behavior when set fails in non-strict mode
            set: function(receiver, name, val) { obj[name] = val; return true; },
            enumerate: function() {
                var result = [];
                for (name in obj) { result.push(name); };
                return result;
            },
            keys: function() { return Object.keys(obj); }
        };
    }

    var hasOwnProperty = ({}).hasOwnProperty;

    function hasOwn(obj, name) {
        return hasOwnProperty.call(obj, name);
    }

    function Dict(table, size) {
        this.table = table || Object.create(null, {});
        this.size = size || 0;
    }

    Dict.create = function(table) {
        var init = Object.create(null, {});
        var size = 0;
        var names = Object.getOwnPropertyNames(table);
        for (var i = 0, n = names.length; i < n; i++) {
            var name = names[i];
            init[name] = table[name];
            size++;
        }
        return new Dict(init, size);
    };

    Dict.prototype = {
        has: function(x) { return hasOwnProperty.call(this.table, x); },
        set: function(x, v) {
            if (!hasOwnProperty.call(this.table, x))
                this.size++;
            this.table[x] = v;
        },
        get: function(x) { return this.table[x]; },
        getDef: function(x, thunk) {
            if (!hasOwnProperty.call(this.table, x)) {
                this.size++;
                this.table[x] = thunk();
            }
            return this.table[x];
        },
        forEach: function(f) {
            var table = this.table;
            for (var key in table)
                f.call(this, key, table[key]);
        },
        map: function(f) {
            var table1 = this.table;
            var table2 = Object.create(null, {});
            this.forEach(function(key, val) {
                table2[key] = f.call(this, val, key);
            });
            return new Dict(table2, this.size);
        },
        mapObject: function(f) {
            var table1 = this.table;
            var table2 = Object.create(null, {});
            this.forEach(function(key, val) {
                table2[key] = f.call(this, val, key);
            });
            return table2;
        },
        toObject: function() {
            return this.mapObject(function(val) { return val; });
        },
        choose: function() {
            return Object.getOwnPropertyNames(this.table)[0];
        },
        remove: function(x) {
            if (hasOwnProperty.call(this.table, x)) {
                this.size--;
                delete this.table[x];
            }
        },
        copy: function() {
            var table = Object.create(null, {});
            for (var key in this.table)
                table[key] = this.table[key];
            return new Dict(table, this.size);
        },
        keys: function() {
            return Object.keys(this.table);
        },
        toString: function() { return "[object Dict]" }
    };

    // shim for ES6 WeakMap with poor asymptotics
    function WeakMap(array) {
        this.array = array || [];
    }

    function searchMap(map, key, found, notFound) {
        var a = map.array;
        for (var i = 0, n = a.length; i < n; i++) {
            var pair = a[i];
            if (pair.key === key)
                return found(pair, i);
        }
        return notFound();
    }

    WeakMap.prototype = {
        has: function(x) {
            return searchMap(this, x, function() { return true }, function() { return false });
        },
        set: function(x, v) {
            var a = this.array;
            searchMap(this, x,
                      function(pair) { pair.value = v },
                      function() { a.push({ key: x, value: v }) });
        },
        get: function(x) {
            return searchMap(this, x,
                             function(pair) { return pair.value },
                             function() { return null });
        },
        "delete": function(x) {
            var a = this.array;
            searchMap(this, x,
                      function(pair, i) { a.splice(i, 1) },
                      function() { });
        },
        toString: function() { return "[object WeakMap]" }
    };

    // non-destructive stack
    function Stack(elts) {
        this.elts = elts || null;
    }

    Stack.prototype = {
        push: function(x) {
            return new Stack({ top: x, rest: this.elts });
        },
        top: function() {
            if (!this.elts)
                throw new Error("empty stack");
            return this.elts.top;
        },
        isEmpty: function() {
            return this.top === null;
        },
        find: function(test) {
            for (var elts = this.elts; elts; elts = elts.rest) {
                if (test(elts.top))
                    return elts.top;
            }
            return null;
        },
        has: function(x) {
            return Boolean(this.find(function(elt) { return elt === x }));
        },
        forEach: function(f) {
            for (var elts = this.elts; elts; elts = elts.rest) {
                f(elts.top);
            }
        }
    };

    if (!Array.prototype.copy) {
        defineProperty(Array.prototype, "copy",
                       function() {
                           var result = [];
                           for (var i = 0, n = this.length; i < n; i++)
                               result[i] = this[i];
                           return result;
                       }, false, false, true);
    }

    if (!Array.prototype.top) {
        defineProperty(Array.prototype, "top",
                       function() {
                           return this.length && this[this.length-1];
                       }, false, false, true);
    }

    return {
        tokens: tokens,
        whitespace: whitespace,
        opTypeNames: opTypeNames,
        keywords: keywords,
        isStatementStartCode: isStatementStartCode,
        tokenIds: tokenIds,
        consts: consts,
        assignOps: assignOps,
        defineGetter: defineGetter,
        defineGetterSetter: defineGetterSetter,
        defineMemoGetter: defineMemoGetter,
        defineProperty: defineProperty,
        isNativeCode: isNativeCode,
        apply: apply,
        applyNew: applyNew,
        mirrorHandler: mirrorHandler,
        mixinHandler: mixinHandler,
        whitelistHandler: whitelistHandler,
        blacklistHandler: blacklistHandler,
        makePassthruHandler: makePassthruHandler,
        Dict: Dict,
        WeakMap: hostGlobal.WeakMap || WeakMap,
        Stack: Stack
    };
}(this));
;
/* vim: set sw=4 ts=4 et tw=78: */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is the Narcissus JavaScript engine.
 *
 * The Initial Developer of the Original Code is
 * Brendan Eich <brendan@mozilla.org>.
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Tom Austin <taustin@ucsc.edu>
 *   Brendan Eich <brendan@mozilla.org>
 *   Shu-Yu Guo <shu@rfrn.org>
 *   Stephan Herhut <stephan.a.herhut@intel.com>
 *   Dave Herman <dherman@mozilla.com>
 *   Dimitris Vardoulakis <dimvar@ccs.neu.edu>
 *   Patrick Walton <pcwalton@mozilla.com>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

/*
 * Narcissus - JS implemented in JS.
 *
 * Lexical scanner.
 */

Narcissus.lexer = (function() {

    var definitions = Narcissus.definitions;

    // Set constants in the local scope.
    eval(definitions.consts);

    // Banned keywords by language version
    const blackLists = { 160: {}, 185: {}, harmony: {} };
    blackLists[160][LET] = true;
    blackLists[160][MODULE] = true;
    blackLists[160][YIELD] = true;
    blackLists[185][MODULE] = true;

    // Build up a trie of operator tokens.
    var opTokens = {};
    for (var op in definitions.opTypeNames) {
        if (op === '\n' || op === '.')
            continue;

        var node = opTokens;
        for (var i = 0; i < op.length; i++) {
            var ch = op[i];
            if (!(ch in node))
                node[ch] = {};
            node = node[ch];
            node.op = op;
        }
    }

    /*
     * Since JavaScript provides no convenient way to determine if a
     * character is in a particular Unicode category, we use
     * metacircularity to accomplish this (oh yeaaaah!)
     */
    function isValidIdentifierChar(ch, first) {
        // check directly for ASCII
        if (ch <= "\u007F") {
            if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch === '$' || ch === '_' ||
                (!first && (ch >= '0' && ch <= '9'))) {
                return true;
            }
            return false;
        }

        // create an object to test this in
        var x = {};
        x["x"+ch] = true;
        x[ch] = true;

        // then use eval to determine if it's a valid character
        var valid = false;
        try {
            valid = (Function("x", "return (x." + (first?"":"x") + ch + ");")(x) === true);
        } catch (ex) {}

        return valid;
    }

    function isIdentifier(str) {
        if (typeof str !== "string")
            return false;

        if (str.length === 0)
            return false;

        if (!isValidIdentifierChar(str[0], true))
            return false;

        for (var i = 1; i < str.length; i++) {
            if (!isValidIdentifierChar(str[i], false))
                return false;
        }

        return true;
    }

    /*
     * Tokenizer :: (source, filename, line number) -> Tokenizer
     */
    function Tokenizer(s, f, l) {
        this.cursor = 0;
        this.source = String(s);
        this.tokens = [];
        this.tokenIndex = 0;
        this.lookahead = 0;
        this.scanNewlines = false;
        this.unexpectedEOF = false;
        this.filename = f || "";
        this.lineno = l || 1;
        this.blackList = blackLists[Narcissus.options.version];
        this.blockComments = null;
    }

    Tokenizer.prototype = {
        get done() {
            // We need to set scanOperand to true here because the first thing
            // might be a regexp.
            return this.peek(true) === END;
        },

        get token() {
            return this.tokens[this.tokenIndex];
        },

        match: function (tt, scanOperand, keywordIsName) {
            return this.get(scanOperand, keywordIsName) === tt || this.unget();
        },

        mustMatch: function (tt, keywordIsName) {
            if (!this.match(tt, false, keywordIsName)) {
                throw this.newSyntaxError("Missing " +
                                          definitions.tokens[tt].toLowerCase());
            }
            return this.token;
        },

        peek: function (scanOperand) {
            var tt, next;
            if (this.lookahead) {
                next = this.tokens[(this.tokenIndex + this.lookahead) & 3];
                tt = (this.scanNewlines && next.lineno !== this.lineno)
                     ? NEWLINE
                     : next.type;
            } else {
                tt = this.get(scanOperand);
                this.unget();
            }
            return tt;
        },

        peekOnSameLine: function (scanOperand) {
            this.scanNewlines = true;
            var tt = this.peek(scanOperand);
            this.scanNewlines = false;
            return tt;
        },

        lastBlockComment: function() {
            var length = this.blockComments.length;
            return length ? this.blockComments[length - 1] : null;
        },

        // Eat comments and whitespace.
        skip: function () {
            var input = this.source;
            this.blockComments = [];
            for (;;) {
                var ch = input[this.cursor++];
                var next = input[this.cursor];
                // handle \r, \r\n and (always preferable) \n
                if (ch === '\r') {
                    // if the next character is \n, we don't care about this at all
                    if (next === '\n') continue;

                    // otherwise, we want to consider this as a newline
                    ch = '\n';
                }

                if (ch === '\n' && !this.scanNewlines) {
                    this.lineno++;
                } else if (ch === '/' && next === '*') {
                    var commentStart = ++this.cursor;
                    for (;;) {
                        ch = input[this.cursor++];
                        if (ch === undefined)
                            throw this.newSyntaxError("Unterminated comment");

                        if (ch === '*') {
                            next = input[this.cursor];
                            if (next === '/') {
                                var commentEnd = this.cursor - 1;
                                this.cursor++;
                                break;
                            }
                        } else if (ch === '\n') {
                            this.lineno++;
                        }
                    }
                    this.blockComments.push(input.substring(commentStart, commentEnd));
                } else if ((ch === '/' && next === '/') ||
                           (Narcissus.options.allowHTMLComments && ch === '<' && next === '!' &&
                            input[this.cursor + 1] === '-' && input[this.cursor + 2] === '-' &&
                            (this.cursor += 2))) {
                    this.cursor++;
                    for (;;) {
                        ch = input[this.cursor++];
                        next = input[this.cursor];
                        if (ch === undefined)
                            return;

                        if (ch === '\r') {
                            // check for \r\n
                            if (next !== '\n') ch = '\n';
                        }

                        if (ch === '\n') {
                            if (this.scanNewlines) {
                                this.cursor--;
                            } else {
                                this.lineno++;
                            }
                            break;
                        }
                    }
                } else if (!(ch in definitions.whitespace)) {
                    this.cursor--;
                    return;
                }
            }
        },

        // Lex the exponential part of a number, if present. Return true iff an
        // exponential part was found.
        lexExponent: function() {
            var input = this.source;
            var next = input[this.cursor];
            if (next === 'e' || next === 'E') {
                this.cursor++;
                ch = input[this.cursor++];
                if (ch === '+' || ch === '-')
                    ch = input[this.cursor++];

                if (ch < '0' || ch > '9')
                    throw this.newSyntaxError("Missing exponent");

                do {
                    ch = input[this.cursor++];
                } while (ch >= '0' && ch <= '9');
                this.cursor--;

                return true;
            }

            return false;
        },

        lexZeroNumber: function (ch) {
            var token = this.token, input = this.source;
            token.type = NUMBER;

            ch = input[this.cursor++];
            if (ch === '.') {
                do {
                    ch = input[this.cursor++];
                } while (ch >= '0' && ch <= '9');
                this.cursor--;

                this.lexExponent();
                token.value = parseFloat(
                                input.substring(token.start, this.cursor));
            } else if (ch === 'x' || ch === 'X') {
                do {
                    ch = input[this.cursor++];
                } while ((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') ||
                         (ch >= 'A' && ch <= 'F'));
                this.cursor--;

                token.value = parseInt(input.substring(token.start, this.cursor));
            } else if (ch >= '0' && ch <= '7') {
                do {
                    ch = input[this.cursor++];
                } while (ch >= '0' && ch <= '7');
                this.cursor--;

                token.value = parseInt(input.substring(token.start, this.cursor));
            } else {
                this.cursor--;
                this.lexExponent();     // 0E1, &c.
                token.value = 0;
            }
        },

        lexNumber: function (ch) {
            var token = this.token, input = this.source;
            token.type = NUMBER;

            var floating = false;
            do {
                ch = input[this.cursor++];
                if (ch === '.' && !floating) {
                    floating = true;
                    ch = input[this.cursor++];
                }
            } while (ch >= '0' && ch <= '9');

            this.cursor--;

            var exponent = this.lexExponent();
            floating = floating || exponent;

            var str = input.substring(token.start, this.cursor);
            token.value = floating ? parseFloat(str) : parseInt(str);
        },

        lexDot: function (ch) {
            var token = this.token, input = this.source;
            var next = input[this.cursor];
            if (next >= '0' && next <= '9') {
                do {
                    ch = input[this.cursor++];
                } while (ch >= '0' && ch <= '9');
                this.cursor--;

                this.lexExponent();

                token.type = NUMBER;
                token.value = parseFloat(
                                input.substring(token.start, this.cursor));
            } else {
                token.type = DOT;
                token.assignOp = null;
                token.value = '.';
            }
        },

        lexString: function (ch) {
            var token = this.token, input = this.source;
            token.type = STRING;

            var hasEscapes = false;
            var delim = ch;
            if (input.length <= this.cursor)
                throw this.newSyntaxError("Unterminated string literal");
            while ((ch = input[this.cursor++]) !== delim) {
                if (this.cursor == input.length)
                    throw this.newSyntaxError("Unterminated string literal");
                if (ch === '\\') {
                    hasEscapes = true;
                    if (++this.cursor == input.length)
                        throw this.newSyntaxError("Unterminated string literal");
                }
            }

            token.value = hasEscapes
                          ? eval(input.substring(token.start, this.cursor))
                          : input.substring(token.start + 1, this.cursor - 1);
        },

        lexRegExp: function (ch) {
            var token = this.token, input = this.source;
            token.type = REGEXP;

            do {
                ch = input[this.cursor++];
                if (ch === '\\') {
                    this.cursor++;
                } else if (ch === '[') {
                    do {
                        if (ch === undefined)
                            throw this.newSyntaxError("Unterminated character class");

                        if (ch === '\\')
                            this.cursor++;

                        ch = input[this.cursor++];
                    } while (ch !== ']');
                } else if (ch === undefined) {
                    throw this.newSyntaxError("Unterminated regex");
                }
            } while (ch !== '/');

            do {
                ch = input[this.cursor++];
            } while (ch >= 'a' && ch <= 'z');

            this.cursor--;

            token.value = eval(input.substring(token.start, this.cursor));
        },

        lexOp: function (ch) {
            var token = this.token, input = this.source;

            // A bit ugly, but it seems wasteful to write a trie lookup routine
            // for only 3 characters...
            var node = opTokens[ch];
            var next = input[this.cursor];
            if (next in node) {
                node = node[next];
                this.cursor++;
                next = input[this.cursor];
                if (next in node) {
                    node = node[next];
                    this.cursor++;
                    next = input[this.cursor];
                }
            }

            var op = node.op;
            if (definitions.assignOps[op] && input[this.cursor] === '=') {
                this.cursor++;
                token.type = ASSIGN;
                token.assignOp = definitions.tokenIds[definitions.opTypeNames[op]];
                op += '=';
            } else {
                token.type = definitions.tokenIds[definitions.opTypeNames[op]];
                token.assignOp = null;
            }

            token.value = op;
        },

        // FIXME: Unicode escape sequences
        lexIdent: function (ch, keywordIsName) {
            var token = this.token;
            var id = ch;

            while ((ch = this.getValidIdentifierChar(false)) !== null) {
                id += ch;
            }

            token.type = IDENTIFIER;
            token.value = id;

            if (keywordIsName)
                return;

            var kw = definitions.keywords[id];
            if (kw && !(kw in this.blackList))
                token.type = kw;
        },

        /*
         * Tokenizer.get :: [boolean[, boolean]] -> token type
         *
         * Consume input *only* if there is no lookahead.
         * Dispatch to the appropriate lexing function depending on the input.
         */
        get: function (scanOperand, keywordIsName) {
            var token;
            while (this.lookahead) {
                --this.lookahead;
                this.tokenIndex = (this.tokenIndex + 1) & 3;
                token = this.tokens[this.tokenIndex];
                if (token.type !== NEWLINE || this.scanNewlines)
                    return token.type;
            }

            this.skip();

            this.tokenIndex = (this.tokenIndex + 1) & 3;
            token = this.tokens[this.tokenIndex];
            if (!token)
                this.tokens[this.tokenIndex] = token = {};

            var input = this.source;
            if (this.cursor >= input.length)
                return token.type = END;

            token.start = this.cursor;
            token.lineno = this.lineno;

            var ich = this.getValidIdentifierChar(true);
            var ch = (ich === null) ? input[this.cursor++] : null;
            if (ich !== null) {
                this.lexIdent(ich, keywordIsName);
            } else if (scanOperand && ch === '/') {
                this.lexRegExp(ch);
            } else if (ch in opTokens) {
                this.lexOp(ch);
            } else if (ch === '.') {
                this.lexDot(ch);
            } else if (ch >= '1' && ch <= '9') {
                this.lexNumber(ch);
            } else if (ch === '0') {
                this.lexZeroNumber(ch);
            } else if (ch === '"' || ch === "'") {
                this.lexString(ch);
            } else if (this.scanNewlines && (ch === '\n' || ch === '\r')) {
                // if this was a \r, look for \r\n
                if (ch === '\r' && input[this.cursor] === '\n') this.cursor++;
                token.type = NEWLINE;
                token.value = '\n';
                this.lineno++;
            } else {
                throw this.newSyntaxError("Illegal token");
            }

            token.end = this.cursor;
            return token.type;
        },

        /*
         * Tokenizer.unget :: void -> undefined
         *
         * Match depends on unget returning undefined.
         */
        unget: function () {
            if (++this.lookahead === 4) throw "PANIC: too much lookahead!";
            this.tokenIndex = (this.tokenIndex - 1) & 3;
        },

        newSyntaxError: function (m) {
            m = (this.filename ? this.filename + ":" : "") + this.lineno + ": " + m;
            var e = new SyntaxError(m, this.filename, this.lineno);
            e.source = this.source;
            e.cursor = this.lookahead
                       ? this.tokens[(this.tokenIndex + this.lookahead) & 3].start
                       : this.cursor;
            return e;
        },


        /* Gets a single valid identifier char from the input stream, or null
         * if there is none.
         */
        getValidIdentifierChar: function(first) {
            var input = this.source;
            if (this.cursor >= input.length) return null;
            var ch = input[this.cursor];

            // first check for \u escapes
            if (ch === '\\' && input[this.cursor+1] === 'u') {
                // get the character value
                try {
                    ch = String.fromCharCode(parseInt(
                        input.substring(this.cursor + 2, this.cursor + 6),
                        16));
                } catch (ex) {
                    return null;
                }
                this.cursor += 5;
            }

            var valid = isValidIdentifierChar(ch, first);
            if (valid) this.cursor++;
            return (valid ? ch : null);
        },
    };


    return {
        isIdentifier: isIdentifier,
        Tokenizer: Tokenizer
    };

}());
;
/* -*- Mode: JS; tab-width: 4; indent-tabs-mode: nil; -*-
 * vim: set sw=4 ts=4 et tw=78:
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is the Narcissus JavaScript engine.
 *
 * The Initial Developer of the Original Code is
 * Brendan Eich <brendan@mozilla.org>.
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Tom Austin <taustin@ucsc.edu>
 *   Brendan Eich <brendan@mozilla.org>
 *   Shu-Yu Guo <shu@rfrn.org>
 *   Dave Herman <dherman@mozilla.com>
 *   Dimitris Vardoulakis <dimvar@ccs.neu.edu>
 *   Patrick Walton <pcwalton@mozilla.com>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

/*
 * Narcissus - JS implemented in JS.
 *
 * Parser.
 */

Narcissus.parser = (function() {

    var lexer = Narcissus.lexer;
    var definitions = Narcissus.definitions;

    const Dict = definitions.Dict;
    const Stack = definitions.Stack;

    // Set constants in the local scope.
    eval(definitions.consts);

    // Banned statement types by language version.
    const blackLists = { 160: {}, 185: {}, harmony: {} };
    blackLists[160][IMPORT] = true;
    blackLists[160][EXPORT] = true;
    blackLists[160][LET] = true;
    blackLists[160][MODULE] = true;
    blackLists[160][YIELD] = true;
    blackLists[185][IMPORT] = true;
    blackLists[185][EXPORT] = true;
    blackLists[185][MODULE] = true;
    blackLists.harmony[WITH] = true;

    /*
     * pushDestructuringVarDecls :: (node, hoisting node) -> void
     *
     * Recursively add all destructured declarations to varDecls.
     */
    function pushDestructuringVarDecls(n, s) {
        for (var i in n) {
            var sub = n[i];
            if (sub.type === IDENTIFIER) {
                s.varDecls.push(sub);
            } else {
                pushDestructuringVarDecls(sub, s);
            }
        }
    }

    function StaticContext(parentScript, parentBlock, inModule, inFunction) {
        this.parentScript = parentScript;
        this.parentBlock = parentBlock || parentScript;
        this.inModule = inModule || false;
        this.inFunction = inFunction || false;
        this.inForLoopInit = false;
        this.topLevel = true;
        this.allLabels = new Stack();
        this.currentLabels = new Stack();
        this.labeledTargets = new Stack();
        this.defaultLoopTarget = null;
        this.defaultTarget = null;
        this.blackList = blackLists[Narcissus.options.version];
        Narcissus.options.ecma3OnlyMode && (this.ecma3OnlyMode = true);
        Narcissus.options.parenFreeMode && (this.parenFreeMode = true);
    }

    StaticContext.prototype = {
        ecma3OnlyMode: false,
        parenFreeMode: false,
        // non-destructive update via prototype extension
        update: function(ext) {
            var desc = {};
            for (var key in ext) {
                desc[key] = {
                    value: ext[key],
                    writable: true,
                    enumerable: true,
                    configurable: true
                }
            }
            return Object.create(this, desc);
        },
        pushLabel: function(label) {
            return this.update({ currentLabels: this.currentLabels.push(label),
                                 allLabels: this.allLabels.push(label) });
        },
        pushTarget: function(target) {
            var isDefaultLoopTarget = target.isLoop;
            var isDefaultTarget = isDefaultLoopTarget || target.type === SWITCH;

            if (this.currentLabels.isEmpty()) {
                if (isDefaultLoopTarget) this.update({ defaultLoopTarget: target });
                if (isDefaultTarget) this.update({ defaultTarget: target });
                return this;
            }

            target.labels = new Dict();
            this.currentLabels.forEach(function(label) {
                target.labels.set(label, true);
            });
            return this.update({ currentLabels: new Stack(),
                                 labeledTargets: this.labeledTargets.push(target),
                                 defaultLoopTarget: isDefaultLoopTarget
                                                    ? target
                                                    : this.defaultLoopTarget,
                                 defaultTarget: isDefaultTarget
                                                ? target
                                                : this.defaultTarget });
        },
        nest: function() {
            return this.topLevel ? this.update({ topLevel: false }) : this;
        },
        allow: function(type) {
            switch (type) {
              case EXPORT:
                if (!this.inModule || this.inFunction || !this.topLevel)
                    return false;
                // FALL THROUGH

              case IMPORT:
                return !this.inFunction && this.topLevel;

              case MODULE:
                return !this.inFunction && this.topLevel;

              default:
                return true;
            }
        }
    };

    /*
     * Script :: (tokenizer, boolean, boolean) -> node
     *
     * Parses the toplevel and module/function bodies.
     */
    function Script(t, inModule, inFunction) {
        var n = new Node(t, scriptInit());
        Statements(t, new StaticContext(n, n, inModule, inFunction), n);
        return n;
    }

    /*
     * Node :: (tokenizer, optional init object) -> node
     */
    function Node(t, init) {
        var token = t.token;
        if (token) {
            // If init.type exists it will override token.type.
            this.type = token.type;
            this.value = token.value;
            this.lineno = token.lineno;

            // Start and end are file positions for error handling.
            this.start = token.start;
            this.end = token.end;
        } else {
            this.lineno = t.lineno;
        }

        // Node uses a tokenizer for debugging (getSource, filename getter).
        this.tokenizer = t;
        this.children = [];

        for (var prop in init)
            this[prop] = init[prop];
    }

    /*
     * SyntheticNode :: (tokenizer, optional init object) -> node
     */
    function SyntheticNode(t, init) {
        // print("SYNTHETIC NODE");
        // if (init.type === COMMA) {
        //     print("SYNTHETIC COMMA");
        //     print(init);
        // }
        this.tokenizer = t;
        this.children = [];
        for (var prop in init)
            this[prop] = init[prop];
        this.synthetic = true;
    }

    var Np = Node.prototype = SyntheticNode.prototype = {};
    Np.constructor = Node;

    const TO_SOURCE_SKIP = {
        type: true,
        value: true,
        lineno: true,
        start: true,
        end: true,
        tokenizer: true,
        assignOp: true
    };
    function unevalableConst(code) {
        var token = definitions.tokens[code];
        var constName = definitions.opTypeNames.hasOwnProperty(token)
                      ? definitions.opTypeNames[token]
                      : token in definitions.keywords
                      ? token.toUpperCase()
                      : token;
        return { toSource: function() { return constName } };
    }
    Np.toSource = function toSource() {
        var mock = {};
        var self = this;
        mock.type = unevalableConst(this.type);
        // avoid infinite recursion in case of back-links
        if (this.generatingSource)
            return mock.toSource();
        this.generatingSource = true;
        if ("value" in this)
            mock.value = this.value;
        if ("lineno" in this)
            mock.lineno = this.lineno;
        if ("start" in this)
            mock.start = this.start;
        if ("end" in this)
            mock.end = this.end;
        if (this.assignOp)
            mock.assignOp = unevalableConst(this.assignOp);
        for (var key in this) {
            if (this.hasOwnProperty(key) && !(key in TO_SOURCE_SKIP))
                mock[key] = this[key];
        }
        try {
            return mock.toSource();
        } finally {
            delete this.generatingSource;
        }
    };

    // Always use push to add operands to an expression, to update start and end.
    Np.push = function (kid) {
        // kid can be null e.g. [1, , 2].
        if (kid !== null) {
            if (kid.start < this.start)
                this.start = kid.start;
            if (this.end < kid.end)
                this.end = kid.end;
        }
        return this.children.push(kid);
    }

    Node.indentLevel = 0;

    function tokenString(tt) {
        var t = definitions.tokens[tt];
        return /^\W/.test(t) ? definitions.opTypeNames[t] : t.toUpperCase();
    }

    Np.toString = function () {
        var a = [];
        for (var i in this) {
            if (this.hasOwnProperty(i) && i !== 'type' && i !== 'target')
                a.push({id: i, value: this[i]});
        }
        a.sort(function (a,b) { return (a.id < b.id) ? -1 : 1; });
        const INDENTATION = "    ";
        var n = ++Node.indentLevel;
        var s = "{\n" + INDENTATION.repeat(n) + "type: " + tokenString(this.type);
        for (i = 0; i < a.length; i++)
            s += ",\n" + INDENTATION.repeat(n) + a[i].id + ": " + a[i].value;
        n = --Node.indentLevel;
        s += "\n" + INDENTATION.repeat(n) + "}";
        return s;
    }

    Np.getSource = function () {
        return this.tokenizer.source.slice(this.start, this.end);
    };

    Np.synth = function(init) {
        var node = new SyntheticNode(this.tokenizer, init);
        node.filename = this.filename;
        node.lineno = this.lineno;
        node.start = this.start;
        node.end = this.end;
        return node;
    };

    /*
     * Helper init objects for common nodes.
     */

    const LOOP_INIT = { isLoop: true };

    function blockInit() {
        return { type: BLOCK, varDecls: [] };
    }

    function scriptInit() {
        return { type: SCRIPT,
                 funDecls: [],
                 varDecls: [],
                 modDefns: new Dict(),
                 modAssns: new Dict(),
                 modDecls: new Dict(),
                 modLoads: new Dict(),
                 impDecls: [],
                 expDecls: [],
                 exports: new Dict(),
                 hasEmptyReturn: false,
                 hasReturnWithValue: false,
                 hasYield: false };
    }

    definitions.defineGetter(Np, "filename",
                             function() {
                                 return this.tokenizer.filename;
                             });

    definitions.defineGetter(Np, "length",
                             function() {
                                 throw new Error("Node.prototype.length is gone; " +
                                                 "use n.children.length instead");
                             });

    definitions.defineProperty(String.prototype, "repeat",
                               function(n) {
                                   var s = "", t = this + s;
                                   while (--n >= 0)
                                       s += t;
                                   return s;
                               }, false, false, true);

    function MaybeLeftParen(t, x) {
        if (x.parenFreeMode)
            return t.match(LEFT_PAREN) ? LEFT_PAREN : END;
        return t.mustMatch(LEFT_PAREN).type;
    }

    function MaybeRightParen(t, p) {
        if (p === LEFT_PAREN)
            t.mustMatch(RIGHT_PAREN);
    }

    /*
     * Statements :: (tokenizer, compiler context, node) -> void
     *
     * Parses a sequence of Statements.
     */
    function Statements(t, x, n) {
        try {
            while (!t.done && t.peek(true) !== RIGHT_CURLY)
                n.push(Statement(t, x));
        } catch (e) {
            if (t.done)
                t.unexpectedEOF = true;
            throw e;
        }
    }

    function Block(t, x) {
        t.mustMatch(LEFT_CURLY);
        var n = new Node(t, blockInit());
        Statements(t, x.update({ parentBlock: n }).pushTarget(n), n);
        t.mustMatch(RIGHT_CURLY);
        return n;
    }

    const DECLARED_FORM = 0, EXPRESSED_FORM = 1, STATEMENT_FORM = 2;

    /*
     * Export :: (binding node, boolean) -> Export
     *
     * Static semantic representation of a module export.
     */
    function Export(node, isDefinition) {
        this.node = node;                 // the AST node declaring this individual export
        this.isDefinition = isDefinition; // is the node an 'export'-annotated definition?
        this.resolved = null;             // resolved pointer to the target of this export
    }

    /*
     * registerExport :: (Dict, EXPORT node) -> void
     */
    function registerExport(exports, decl) {
        function register(name, exp) {
            if (exports.has(name))
                throw new SyntaxError("multiple exports of " + name);
            exports.set(name, exp);
        }

        switch (decl.type) {
          case MODULE:
          case FUNCTION:
            register(decl.name, new Export(decl, true));
            break;

          case VAR:
            for (var i = 0; i < decl.children.length; i++)
                register(decl.children[i].name, new Export(decl.children[i], true));
            break;

          case LET:
          case CONST:
            throw new Error("NYI: " + definitions.tokens[decl.type]);

          case EXPORT:
            for (var i = 0; i < decl.pathList.length; i++) {
                var path = decl.pathList[i];
                switch (path.type) {
                  case OBJECT_INIT:
                    for (var j = 0; j < path.children.length; j++) {
                        // init :: IDENTIFIER | PROPERTY_INIT
                        var init = path.children[j];
                        if (init.type === IDENTIFIER)
                            register(init.value, new Export(init, false));
                        else
                            register(init.children[0].value, new Export(init.children[1], false));
                    }
                    break;

                  case DOT:
                    register(path.children[1].value, new Export(path, false));
                    break;

                  case IDENTIFIER:
                    register(path.value, new Export(path, false));
                    break;

                  default:
                    throw new Error("unexpected export path: " + definitions.tokens[path.type]);
                }
            }
            break;

          default:
            throw new Error("unexpected export decl: " + definitions.tokens[exp.type]);
        }
    }

    /*
     * Module :: (node) -> Module
     *
     * Static semantic representation of a module.
     */
    function Module(node) {
        var exports = node.body.exports;
        var modDefns = node.body.modDefns;

        var exportedModules = new Dict();

        exports.forEach(function(name, exp) {
            var node = exp.node;
            if (node.type === MODULE) {
                exportedModules.set(name, node);
            } else if (!exp.isDefinition && node.type === IDENTIFIER && modDefns.has(node.value)) {
                var mod = modDefns.get(node.value);
                exportedModules.set(name, mod);
            }
        });

        this.node = node;
        this.exports = exports;
        this.exportedModules = exportedModules;
    }

    /*
     * Statement :: (tokenizer, compiler context) -> node
     *
     * Parses a Statement.
     */
    function Statement(t, x) {
        var i, label, n, n2, p, c, ss, tt = t.get(true), tt2, x2, x3;

        var comments = t.blockComments;

        if (x.blackList[tt])
            throw t.newSyntaxError(definitions.tokens[tt] + " statements only allowed in Harmony");
        if (!x.allow(tt))
            throw t.newSyntaxError(definitions.tokens[tt] + " statement in illegal context");

        // Cases for statements ending in a right curly return early, avoiding the
        // common semicolon insertion magic after this switch.
        switch (tt) {
          case IMPORT:
            n = new Node(t);
            n.pathList = ImportPathList(t, x);
            x.parentScript.impDecls.push(n);
            break;

          case EXPORT:
            switch (t.peek()) {
              case MODULE:
              case FUNCTION:
              case LET:
              case VAR:
              case CONST:
                n = Statement(t, x);
                n.blockComments = comments;
                n.exported = true;
                x.parentScript.expDecls.push(n);
                registerExport(x.parentScript.exports, n);
                return n;

              default:
                n = new Node(t);
                n.pathList = ExportPathList(t, x);
                break;
            }
            x.parentScript.expDecls.push(n);
            registerExport(x.parentScript.exports, n);
            break;

          case MODULE:
            n = new Node(t);
            n.blockComments = comments;
            t.mustMatch(IDENTIFIER);
            label = t.token.value;

            if (t.match(LEFT_CURLY)) {
                n.name = label;
                n.body = Script(t, true, false);
                n.module = new Module(n);
                t.mustMatch(RIGHT_CURLY);
                x.parentScript.modDefns.set(n.name, n);
                return n;
            }

            t.unget();
            ModuleVariables(t, x, n);
            return n;

          case FUNCTION:
            // DECLARED_FORM extends funDecls of x, STATEMENT_FORM doesn't.
            return FunctionDefinition(t, x, true, x.topLevel ? DECLARED_FORM : STATEMENT_FORM, comments);

          case LEFT_CURLY:
            n = new Node(t, blockInit());
            Statements(t, x.update({ parentBlock: n }).pushTarget(n).nest(), n);
            t.mustMatch(RIGHT_CURLY);
            return n;

          case IF:
            n = new Node(t);
            n.condition = HeadExpression(t, x);
            x2 = x.pushTarget(n).nest();
            n.thenPart = Statement(t, x2);
            n.elsePart = t.match(ELSE, true) ? Statement(t, x2) : null;
            return n;

          case SWITCH:
            // This allows CASEs after a DEFAULT, which is in the standard.
            n = new Node(t, { cases: [], defaultIndex: -1 });
            n.discriminant = HeadExpression(t, x);
            x2 = x.pushTarget(n).nest();
            t.mustMatch(LEFT_CURLY);
            while ((tt = t.get()) !== RIGHT_CURLY) {
                switch (tt) {
                  case DEFAULT:
                    if (n.defaultIndex >= 0)
                        throw t.newSyntaxError("More than one switch default");
                    // FALL THROUGH
                  case CASE:
                    n2 = new Node(t);
                    if (tt === DEFAULT)
                        n.defaultIndex = n.cases.length;
                    else
                        n2.caseLabel = Expression(t, x2, COLON);
                    break;

                  default:
                    throw t.newSyntaxError("Invalid switch case");
                }
                t.mustMatch(COLON);
                n2.statements = new Node(t, blockInit());
                while ((tt=t.peek(true)) !== CASE && tt !== DEFAULT &&
                        tt !== RIGHT_CURLY)
                    n2.statements.push(Statement(t, x2));
                n.cases.push(n2);
            }
            return n;

          case FOR:
            n = new Node(t, LOOP_INIT);
            n.blockComments = comments;
            if (t.match(IDENTIFIER)) {
                if (t.token.value === "each")
                    n.isEach = true;
                else
                    t.unget();
            }
            if (!x.parenFreeMode)
                t.mustMatch(LEFT_PAREN);
            x2 = x.pushTarget(n).nest();
            x3 = x.update({ inForLoopInit: true });
            n2 = null;
            if ((tt = t.peek(true)) !== SEMICOLON) {
                if (tt === VAR || tt === CONST) {
                    t.get();
                    n2 = Variables(t, x3);
                } else if (tt === LET) {
                    t.get();
                    if (t.peek() === LEFT_PAREN) {
                        n2 = LetBlock(t, x3, false);
                    } else {
                        // Let in for head, we need to add an implicit block
                        // around the rest of the for.
                        x3.parentBlock = n;
                        n.varDecls = [];
                        n2 = Variables(t, x3);
                    }
                } else {
                    n2 = Expression(t, x3);
                }
            }
            if (n2 && t.match(IN)) {
                n.type = FOR_IN;
                n.object = Expression(t, x3);
                if (n2.type === VAR || n2.type === LET) {
                    c = n2.children;

                    // Destructuring turns one decl into multiples, so either
                    // there must be only one destructuring or only one
                    // decl.
                    if (c.length !== 1 && n2.destructurings.length !== 1) {
                        throw new SyntaxError("Invalid for..in left-hand side",
                                              t.filename, n2.lineno);
                    }
                    if (n2.destructurings.length > 0) {
                        n.iterator = n2.destructurings[0];
                    } else {
                        n.iterator = c[0];
                    }
                    n.varDecl = n2;
                } else {
                    if (n2.type === ARRAY_INIT || n2.type === OBJECT_INIT) {
                        n2.destructuredNames = checkDestructuring(t, x3, n2);
                    }
                    n.iterator = n2;
                }
            } else {
                x3.inForLoopInit = false;
                n.setup = n2;
                t.mustMatch(SEMICOLON);
                if (n.isEach)
                    throw t.newSyntaxError("Invalid for each..in loop");
                n.condition = (t.peek(true) === SEMICOLON)
                              ? null
                              : Expression(t, x3);
                t.mustMatch(SEMICOLON);
                tt2 = t.peek(true);
                n.update = (x.parenFreeMode
                            ? tt2 === LEFT_CURLY || definitions.isStatementStartCode[tt2]
                            : tt2 === RIGHT_PAREN)
                           ? null
                           : Expression(t, x3);
            }
            if (!x.parenFreeMode)
                t.mustMatch(RIGHT_PAREN);
            n.body = Statement(t, x2);
            return n;

          case WHILE:
            n = new Node(t, { isLoop: true });
            n.blockComments = comments;
            n.condition = HeadExpression(t, x);
            n.body = Statement(t, x.pushTarget(n).nest());
            return n;

          case DO:
            n = new Node(t, { isLoop: true });
            n.blockComments = comments;
            n.body = Statement(t, x.pushTarget(n).nest());
            t.mustMatch(WHILE);
            n.condition = HeadExpression(t, x);
            if (!x.ecmaStrictMode) {
                // <script language="JavaScript"> (without version hints) may need
                // automatic semicolon insertion without a newline after do-while.
                // See http://bugzilla.mozilla.org/show_bug.cgi?id=238945.
                t.match(SEMICOLON);
                return n;
            }
            break;

          case BREAK:
          case CONTINUE:
            n = new Node(t);
            n.blockComments = comments;

            // handle the |foo: break foo;| corner case
            x2 = x.pushTarget(n);

            if (t.peekOnSameLine() === IDENTIFIER) {
                t.get();
                n.label = t.token.value;
            }

            if (n.label) {
                n.target = x2.labeledTargets.find(function(target) {
                    return target.labels.has(n.label)
                });
            } else if (tt === CONTINUE) {
                n.target = x2.defaultLoopTarget;
            } else {
                n.target = x2.defaultTarget;
            }

            if (!n.target)
                throw t.newSyntaxError("Invalid " + ((tt === BREAK) ? "break" : "continue"));
            if (!n.target.isLoop && tt === CONTINUE)
                throw t.newSyntaxError("Invalid continue");

            break;

          case TRY:
            n = new Node(t, { catchClauses: [] });
            n.blockComments = comments;
            n.tryBlock = Block(t, x);
            while (t.match(CATCH)) {
                n2 = new Node(t);
                p = MaybeLeftParen(t, x);
                switch (t.get()) {
                  case LEFT_BRACKET:
                  case LEFT_CURLY:
                    // Destructured catch identifiers.
                    t.unget();
                    n2.varName = DestructuringExpression(t, x, true);
                    break;
                  case IDENTIFIER:
                    n2.varName = t.token.value;
                    break;
                  default:
                    throw t.newSyntaxError("missing identifier in catch");
                    break;
                }
                if (t.match(IF)) {
                    if (x.ecma3OnlyMode)
                        throw t.newSyntaxError("Illegal catch guard");
                    if (n.catchClauses.length && !n.catchClauses.top().guard)
                        throw t.newSyntaxError("Guarded catch after unguarded");
                    n2.guard = Expression(t, x);
                }
                MaybeRightParen(t, p);
                n2.block = Block(t, x);
                n.catchClauses.push(n2);
            }
            if (t.match(FINALLY))
                n.finallyBlock = Block(t, x);
            if (!n.catchClauses.length && !n.finallyBlock)
                throw t.newSyntaxError("Invalid try statement");
            return n;

          case CATCH:
          case FINALLY:
            throw t.newSyntaxError(definitions.tokens[tt] + " without preceding try");

          case THROW:
            n = new Node(t);
            n.exception = Expression(t, x);
            break;

          case RETURN:
            n = ReturnOrYield(t, x);
            break;

          case WITH:
            n = new Node(t);
            n.blockComments = comments;
            n.object = HeadExpression(t, x);
            n.body = Statement(t, x.pushTarget(n).nest());
            return n;

          case VAR:
          case CONST:
            n = Variables(t, x);
            break;

          case LET:
            if (t.peek() === LEFT_PAREN) {
                n = LetBlock(t, x, true);
                return n;
            }
            n = Variables(t, x);
            break;

          case DEBUGGER:
            n = new Node(t);
            break;

          case NEWLINE:
          case SEMICOLON:
            n = new Node(t, { type: SEMICOLON });
            n.blockComments = comments;
            n.expression = null;
            return n;

          default:
            if (tt === IDENTIFIER) {
                tt = t.peek();
                // Labeled statement.
                if (tt === COLON) {
                    label = t.token.value;
                    if (x.allLabels.has(label))
                        throw t.newSyntaxError("Duplicate label");
                    t.get();
                    n = new Node(t, { type: LABEL, label: label });
                    n.blockComments = comments;
                    n.statement = Statement(t, x.pushLabel(label).nest());
                    n.target = (n.statement.type === LABEL) ? n.statement.target : n.statement;
                    return n;
                }
            }

            // Expression statement.
            // We unget the current token to parse the expression as a whole.
            n = new Node(t, { type: SEMICOLON });
            t.unget();
            n.blockComments = comments;
            n.expression = Expression(t, x);
            n.end = n.expression.end;
            break;
        }

        n.blockComments = comments;
        MagicalSemicolon(t);
        return n;
    }

    /*
     * MagicalSemicolon :: (tokenizer) -> void
     */
    function MagicalSemicolon(t) {
        var tt;
        if (t.lineno === t.token.lineno) {
            tt = t.peekOnSameLine();
            if (tt !== END && tt !== NEWLINE && tt !== SEMICOLON && tt !== RIGHT_CURLY)
                throw t.newSyntaxError("missing ; before statement");
        }
        t.match(SEMICOLON);
    }

    /*
     * ReturnOrYield :: (tokenizer, compiler context) -> (RETURN | YIELD) node
     */
    function ReturnOrYield(t, x) {
        var n, b, tt = t.token.type, tt2;

        var parentScript = x.parentScript;

        if (tt === RETURN) {
            if (!x.inFunction)
                throw t.newSyntaxError("Return not in function");
        } else /* if (tt === YIELD) */ {
            if (!x.inFunction)
                throw t.newSyntaxError("Yield not in function");
            parentScript.hasYield = true;
        }
        n = new Node(t, { value: undefined });

        tt2 = (tt === RETURN) ? t.peekOnSameLine(true) : t.peek(true);
        if (tt2 !== END && tt2 !== NEWLINE &&
            tt2 !== SEMICOLON && tt2 !== RIGHT_CURLY
            && (tt !== YIELD ||
                (tt2 !== tt && tt2 !== RIGHT_BRACKET && tt2 !== RIGHT_PAREN &&
                 tt2 !== COLON && tt2 !== COMMA))) {
            if (tt === RETURN) {
                n.value = Expression(t, x);
                parentScript.hasReturnWithValue = true;
            } else {
                n.value = AssignExpression(t, x);
            }
        } else if (tt === RETURN) {
            parentScript.hasEmptyReturn = true;
        }

        return n;
    }

    /*
     * ModuleExpression :: (tokenizer, compiler context) -> (STRING | IDENTIFIER | DOT) node
     */
    function ModuleExpression(t, x) {
        return t.match(STRING) ? new Node(t) : QualifiedPath(t, x);
    }

    /*
     * ImportPathList :: (tokenizer, compiler context) -> Array[DOT node]
     */
    function ImportPathList(t, x) {
        var a = [];
        do {
            a.push(ImportPath(t, x));
        } while (t.match(COMMA));
        return a;
    }

    /*
     * ImportPath :: (tokenizer, compiler context) -> DOT node
     */
    function ImportPath(t, x) {
        var n = QualifiedPath(t, x);
        if (!t.match(DOT)) {
            if (n.type === IDENTIFIER)
                throw t.newSyntaxError("cannot import local variable");
            return n;
        }

        var n2 = new Node(t);
        n2.push(n);
        n2.push(ImportSpecifierSet(t, x));
        return n2;
    }

    /*
     * ExplicitSpecifierSet :: (tokenizer, compiler context, (tokenizer, compiler context) -> node)
     *                      -> OBJECT_INIT node
     */
    function ExplicitSpecifierSet(t, x, SpecifierRHS) {
        var n, n2, id, tt;

        n = new Node(t, { type: OBJECT_INIT });
        t.mustMatch(LEFT_CURLY);

        if (!t.match(RIGHT_CURLY)) {
            do {
                id = Identifier(t, x);
                if (t.match(COLON)) {
                    n2 = new Node(t, { type: PROPERTY_INIT });
                    n2.push(id);
                    n2.push(SpecifierRHS(t, x));
                    n.push(n2);
                } else {
                    n.push(id);
                }
            } while (!t.match(RIGHT_CURLY) && t.mustMatch(COMMA));
        }

        return n;
    }

    /*
     * ImportSpecifierSet :: (tokenizer, compiler context) -> (IDENTIFIER | OBJECT_INIT) node
     */
    function ImportSpecifierSet(t, x) {
        return t.match(MUL)
             ? new Node(t, { type: IDENTIFIER, name: "*" })
             : ExplicitSpecifierSet(t, x, Identifier);
    }

    /*
     * Identifier :: (tokenizer, compiler context) -> IDENTIFIER node
     */
    function Identifier(t, x) {
        t.mustMatch(IDENTIFIER);
        return new Node(t, { type: IDENTIFIER });
    }

    /*
     * IdentifierName :: (tokenizer) -> IDENTIFIER node
     */
    function IdentifierName(t) {
        t.mustMatch(IDENTIFIER, true);
        return new Node(t, { type: IDENTIFIER });
    }

    /*
     * QualifiedPath :: (tokenizer, compiler context) -> (IDENTIFIER | DOT) node
     */
    function QualifiedPath(t, x) {
        var n, n2;

        n = Identifier(t, x);

        while (t.match(DOT)) {
            if (t.peek() !== IDENTIFIER) {
                // Unget the '.' token, which isn't part of the QualifiedPath.
                t.unget();
                break;
            }
            n2 = new Node(t);
            n2.push(n);
            n2.push(Identifier(t, x));
            n = n2;
        }

        return n;
    }

    /*
     * ExportPath :: (tokenizer, compiler context) -> (IDENTIFIER | DOT | OBJECT_INIT) node
     */
    function ExportPath(t, x) {
        if (t.peek() === LEFT_CURLY)
            return ExplicitSpecifierSet(t, x, QualifiedPath);
        return QualifiedPath(t, x);
    }

    /*
     * ExportPathList :: (tokenizer, compiler context)
     *                -> Array[(IDENTIFIER | DOT | OBJECT_INIT) node]
     */
    function ExportPathList(t, x) {
        var a = [];
        do {
            a.push(ExportPath(t, x));
        } while (t.match(COMMA));
        return a;
    }

    /*
     * FunctionDefinition :: (tokenizer, compiler context, boolean,
     *                        DECLARED_FORM or EXPRESSED_FORM or STATEMENT_FORM,
     *                        [string] or null or undefined)
     *                    -> node
     */
    function FunctionDefinition(t, x, requireName, functionForm, comments) {
        var tt;
        var f = new Node(t, { params: [], paramComments: [] });
        if (typeof comment === "undefined")
            comment = null;
        f.blockComments = comments;
        if (f.type !== FUNCTION)
            f.type = (f.value === "get") ? GETTER : SETTER;
        if (t.match(MUL))
            f.isExplicitGenerator = true;
        if (t.match(IDENTIFIER, false, true))
            f.name = t.token.value;
        else if (requireName)
            throw t.newSyntaxError("missing function identifier");

        var inModule = x ? x.inModule : false;
        var x2 = new StaticContext(null, null, inModule, true);

        t.mustMatch(LEFT_PAREN);
        if (!t.match(RIGHT_PAREN)) {
            do {
                tt = t.get();
                f.paramComments.push(t.lastBlockComment());
                switch (tt) {
                  case LEFT_BRACKET:
                  case LEFT_CURLY:
                    // Destructured formal parameters.
                    t.unget();
                    f.params.push(DestructuringExpression(t, x2));
                    break;
                  case IDENTIFIER:
                    f.params.push(t.token.value);
                    break;
                  default:
                    throw t.newSyntaxError("missing formal parameter");
                    break;
                }
            } while (t.match(COMMA));
            t.mustMatch(RIGHT_PAREN);
        }

        // Do we have an expression closure or a normal body?
        tt = t.get(true);
        if (tt !== LEFT_CURLY)
            t.unget();

        if (tt !== LEFT_CURLY) {
            f.body = AssignExpression(t, x2);
        } else {
            f.body = Script(t, inModule, true);
        }

        if (tt === LEFT_CURLY)
            t.mustMatch(RIGHT_CURLY);

        f.end = t.token.end;
        f.functionForm = functionForm;
        if (functionForm === DECLARED_FORM)
            x.parentScript.funDecls.push(f);

        if (Narcissus.options.version === "harmony" && !f.isExplicitGenerator && f.body.hasYield)
            throw t.newSyntaxError("yield in non-generator function");

        if (f.isExplicitGenerator || f.body.hasYield)
            f.body = new Node(t, { type: GENERATOR, body: f.body });

        return f;
    }

    /*
     * ModuleVariables :: (tokenizer, compiler context, MODULE node) -> void
     *
     * Parses a comma-separated list of module declarations (and maybe
     * initializations).
     */
    function ModuleVariables(t, x, n) {
        var n1, n2;
        do {
            n1 = Identifier(t, x);
            if (t.match(ASSIGN)) {
                n2 = ModuleExpression(t, x);
                n1.initializer = n2;
                if (n2.type === STRING)
                    x.parentScript.modLoads.set(n1.value, n2.value);
                else
                    x.parentScript.modAssns.set(n1.value, n1);
            }
            n.push(n1);
        } while (t.match(COMMA));
    }

    /*
     * Variables :: (tokenizer, compiler context) -> node
     *
     * Parses a comma-separated list of var declarations (and maybe
     * initializations).
     */
    function Variables(t, x, letBlock) {
        var n, n2, ss, i, s, tt;

        tt = t.token.type;
        switch (tt) {
          case VAR:
          case CONST:
            s = x.parentScript;
            break;
          case LET:
            s = x.parentBlock;
            break;
          case LEFT_PAREN:
            tt = LET;
            s = letBlock;
            break;
        }

        n = new Node(t, { type: tt, destructurings: [] });

        do {
            tt = t.get();
            if (tt === LEFT_BRACKET || tt === LEFT_CURLY) {
                // Need to unget to parse the full destructured expression.
                t.unget();

                var dexp = DestructuringExpression(t, x, true);

                n2 = new Node(t, { type: IDENTIFIER,
                                   name: dexp,
                                   readOnly: n.type === CONST });
                n.push(n2);
                pushDestructuringVarDecls(n2.name.destructuredNames, s);
                n.destructurings.push({ exp: dexp, decl: n2 });

                if (x.inForLoopInit && t.peek() === IN) {
                    continue;
                }

                t.mustMatch(ASSIGN);
                if (t.token.assignOp)
                    throw t.newSyntaxError("Invalid variable initialization");

                n2.blockComment = t.lastBlockComment();
                n2.initializer = AssignExpression(t, x);

                continue;
            }

            if (tt !== IDENTIFIER)
                throw t.newSyntaxError("missing variable name");

            n2 = new Node(t, { type: IDENTIFIER,
                               name: t.token.value,
                               readOnly: n.type === CONST });
            n.push(n2);
            s.varDecls.push(n2);

            if (t.match(ASSIGN)) {
                var comment = t.lastBlockComment();
                if (t.token.assignOp)
                    throw t.newSyntaxError("Invalid variable initialization");

                n2.initializer = AssignExpression(t, x);
            } else {
                var comment = t.lastBlockComment();
            }
            n2.blockComment = comment;
        } while (t.match(COMMA));

        return n;
    }

    /*
     * LetBlock :: (tokenizer, compiler context, boolean) -> node
     *
     * Does not handle let inside of for loop init.
     */
    function LetBlock(t, x, isStatement) {
        var n, n2;

        // t.token.type must be LET
        n = new Node(t, { type: LET_BLOCK, varDecls: [] });
        t.mustMatch(LEFT_PAREN);
        n.variables = Variables(t, x, n);
        t.mustMatch(RIGHT_PAREN);

        if (isStatement && t.peek() !== LEFT_CURLY) {
            /*
             * If this is really an expression in let statement guise, then we
             * need to wrap the LET_BLOCK node in a SEMICOLON node so that we pop
             * the return value of the expression.
             */
            n2 = new Node(t, { type: SEMICOLON,
                               expression: n });
            isStatement = false;
        }

        if (isStatement)
            n.block = Block(t, x);
        else
            n.expression = AssignExpression(t, x);

        return n;
    }

    function checkDestructuring(t, x, n, simpleNamesOnly) {
        if (n.type === ARRAY_COMP)
            throw t.newSyntaxError("Invalid array comprehension left-hand side");
        if (n.type !== ARRAY_INIT && n.type !== OBJECT_INIT)
            return;

        var lhss = {};
        var nn, n2, idx, sub, cc, c = n.children;
        for (var i = 0, j = c.length; i < j; i++) {
            if (!(nn = c[i]))
                continue;
            if (nn.type === PROPERTY_INIT) {
                cc = nn.children;
                sub = cc[1];
                idx = cc[0].value;
            } else if (n.type === OBJECT_INIT) {
                // Do we have destructuring shorthand {foo, bar}?
                sub = nn;
                idx = nn.value;
            } else {
                sub = nn;
                idx = i;
            }

            if (sub.type === ARRAY_INIT || sub.type === OBJECT_INIT) {
                lhss[idx] = checkDestructuring(t, x, sub, simpleNamesOnly);
            } else {
                if (simpleNamesOnly && sub.type !== IDENTIFIER) {
                    // In declarations, lhs must be simple names
                    throw t.newSyntaxError("missing name in pattern");
                }

                lhss[idx] = sub;
            }
        }

        return lhss;
    }

    function DestructuringExpression(t, x, simpleNamesOnly) {
        var n = PrimaryExpression(t, x);
        // Keep the list of lefthand sides for varDecls
        n.destructuredNames = checkDestructuring(t, x, n, simpleNamesOnly);
        return n;
    }

    function GeneratorExpression(t, x, e) {
        return new Node(t, { type: GENERATOR,
                             expression: e,
                             tail: ComprehensionTail(t, x) });
    }

    function ComprehensionTail(t, x) {
        var body, n, n2, n3, p;

        // t.token.type must be FOR
        body = new Node(t, { type: COMP_TAIL });

        do {
            // Comprehension tails are always for..in loops.
            n = new Node(t, { type: FOR_IN, isLoop: true });
            if (t.match(IDENTIFIER)) {
                // But sometimes they're for each..in.
                if (t.token.value === "each")
                    n.isEach = true;
                else
                    t.unget();
            }
            p = MaybeLeftParen(t, x);
            switch(t.get()) {
              case LEFT_BRACKET:
              case LEFT_CURLY:
                t.unget();
                // Destructured left side of for in comprehension tails.
                n.iterator = DestructuringExpression(t, x);
                break;

              case IDENTIFIER:
                n.iterator = n3 = new Node(t, { type: IDENTIFIER });
                n3.name = n3.value;
                n.varDecl = n2 = new Node(t, { type: VAR });
                n2.push(n3);
                x.parentScript.varDecls.push(n3);
                // Don't add to varDecls since the semantics of comprehensions is
                // such that the variables are in their own function when
                // desugared.
                break;

              default:
                throw t.newSyntaxError("missing identifier");
            }
            t.mustMatch(IN);
            n.object = Expression(t, x);
            MaybeRightParen(t, p);
            body.push(n);
        } while (t.match(FOR));

        // Optional guard.
        if (t.match(IF))
            body.guard = HeadExpression(t, x);

        return body;
    }

    function HeadExpression(t, x) {
        var p = MaybeLeftParen(t, x);
        var n = ParenExpression(t, x);
        MaybeRightParen(t, p);
        if (p === END && !n.parenthesized) {
            var tt = t.peek();
            if (tt !== LEFT_CURLY && !definitions.isStatementStartCode[tt])
                throw t.newSyntaxError("Unparenthesized head followed by unbraced body");
        }
        return n;
    }

    function ParenExpression(t, x) {
        // Always accept the 'in' operator in a parenthesized expression,
        // where it's unambiguous, even if we might be parsing the init of a
        // for statement.
        var n = Expression(t, x.update({ inForLoopInit: x.inForLoopInit &&
                                                        (t.token.type === LEFT_PAREN) }));

        if (t.match(FOR)) {
            if (n.type === YIELD && !n.parenthesized)
                throw t.newSyntaxError("Yield expression must be parenthesized");
            if (n.type === COMMA && !n.parenthesized)
                throw t.newSyntaxError("Generator expression must be parenthesized");
            n = GeneratorExpression(t, x, n);
        }

        return n;
    }

    /*
     * Expression :: (tokenizer, compiler context) -> node
     *
     * Top-down expression parser matched against SpiderMonkey.
     */
    function Expression(t, x) {
        var n, n2;

        n = AssignExpression(t, x);
        if (t.match(COMMA)) {
            n2 = new Node(t, { type: COMMA });
            n2.push(n);
            n = n2;
            do {
                n2 = n.children[n.children.length-1];
                if (n2.type === YIELD && !n2.parenthesized)
                    throw t.newSyntaxError("Yield expression must be parenthesized");
                n.push(AssignExpression(t, x));
            } while (t.match(COMMA));
        }

        return n;
    }

    function AssignExpression(t, x) {
        var n, lhs;

        // Have to treat yield like an operand because it could be the leftmost
        // operand of the expression.
        if (t.match(YIELD, true))
            return ReturnOrYield(t, x);

        n = new Node(t, { type: ASSIGN });
        lhs = ConditionalExpression(t, x);

        if (!t.match(ASSIGN)) {
            return lhs;
        }

        n.blockComment = t.lastBlockComment();

        switch (lhs.type) {
          case OBJECT_INIT:
          case ARRAY_INIT:
            lhs.destructuredNames = checkDestructuring(t, x, lhs);
            // FALL THROUGH
          case IDENTIFIER: case DOT: case INDEX: case CALL:
            break;
          default:
            throw t.newSyntaxError("Bad left-hand side of assignment");
            break;
        }

        n.assignOp = lhs.assignOp = t.token.assignOp;
        n.push(lhs);
        n.push(AssignExpression(t, x));

        return n;
    }

    function ConditionalExpression(t, x) {
        var n, n2;

        n = OrExpression(t, x);
        if (t.match(HOOK)) {
            n2 = n;
            n = new Node(t, { type: HOOK });
            n.push(n2);
            /*
             * Always accept the 'in' operator in the middle clause of a ternary,
             * where it's unambiguous, even if we might be parsing the init of a
             * for statement.
             */
            n.push(AssignExpression(t, x.update({ inForLoopInit: false })));
            if (!t.match(COLON))
                throw t.newSyntaxError("missing : after ?");
            n.push(AssignExpression(t, x));
        }

        return n;
    }

    function OrExpression(t, x) {
        var n, n2;

        n = AndExpression(t, x);
        while (t.match(OR)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(AndExpression(t, x));
            n = n2;
        }

        return n;
    }

    function AndExpression(t, x) {
        var n, n2;

        n = BitwiseOrExpression(t, x);
        while (t.match(AND)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(BitwiseOrExpression(t, x));
            n = n2;
        }

        return n;
    }

    function BitwiseOrExpression(t, x) {
        var n, n2;

        n = BitwiseXorExpression(t, x);
        while (t.match(BITWISE_OR)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(BitwiseXorExpression(t, x));
            n = n2;
        }

        return n;
    }

    function BitwiseXorExpression(t, x) {
        var n, n2;

        n = BitwiseAndExpression(t, x);
        while (t.match(BITWISE_XOR)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(BitwiseAndExpression(t, x));
            n = n2;
        }

        return n;
    }

    function BitwiseAndExpression(t, x) {
        var n, n2;

        n = EqualityExpression(t, x);
        while (t.match(BITWISE_AND)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(EqualityExpression(t, x));
            n = n2;
        }

        return n;
    }

    function EqualityExpression(t, x) {
        var n, n2;

        n = RelationalExpression(t, x);
        while (t.match(EQ) || t.match(NE) ||
               t.match(STRICT_EQ) || t.match(STRICT_NE)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(RelationalExpression(t, x));
            n = n2;
        }

        return n;
    }

    function RelationalExpression(t, x) {
        var n, n2;

        /*
         * Uses of the in operator in shiftExprs are always unambiguous,
         * so unset the flag that prohibits recognizing it.
         */
        var x2 = x.update({ inForLoopInit: false });
        n = ShiftExpression(t, x2);
        while ((t.match(LT) || t.match(LE) || t.match(GE) || t.match(GT) ||
               (!x.inForLoopInit && t.match(IN)) ||
               t.match(INSTANCEOF))) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(ShiftExpression(t, x2));
            n = n2;
        }

        return n;
    }

    function ShiftExpression(t, x) {
        var n, n2;

        n = AddExpression(t, x);
        while (t.match(LSH) || t.match(RSH) || t.match(URSH)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(AddExpression(t, x));
            n = n2;
        }

        return n;
    }

    function AddExpression(t, x) {
        var n, n2;

        n = MultiplyExpression(t, x);
        while (t.match(PLUS) || t.match(MINUS)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(MultiplyExpression(t, x));
            n = n2;
        }

        return n;
    }

    function MultiplyExpression(t, x) {
        var n, n2;

        n = UnaryExpression(t, x);
        while (t.match(MUL) || t.match(DIV) || t.match(MOD)) {
            n2 = new Node(t);
            n2.push(n);
            n2.push(UnaryExpression(t, x));
            n = n2;
        }

        return n;
    }

    function UnaryExpression(t, x) {
        var n, n2, tt;

        switch (tt = t.get(true)) {
          case DELETE: case VOID: case TYPEOF:
          case NOT: case BITWISE_NOT: case PLUS: case MINUS:
            if (tt === PLUS)
                n = new Node(t, { type: UNARY_PLUS });
            else if (tt === MINUS)
                n = new Node(t, { type: UNARY_MINUS });
            else
                n = new Node(t);
            n.push(UnaryExpression(t, x));
            break;

          case INCREMENT:
          case DECREMENT:
            // Prefix increment/decrement.
            n = new Node(t);
            n.push(MemberExpression(t, x, true));
            break;

          default:
            t.unget();
            n = MemberExpression(t, x, true);

            // Don't look across a newline boundary for a postfix {in,de}crement.
            if (t.tokens[(t.tokenIndex + t.lookahead - 1) & 3].lineno ===
                t.lineno) {
                if (t.match(INCREMENT) || t.match(DECREMENT)) {
                    n2 = new Node(t, { postfix: true });
                    n2.push(n);
                    n = n2;
                }
            }
            break;
        }

        return n;
    }

    function MemberExpression(t, x, allowCallSyntax) {
        var n, n2, name, tt;

        if (t.match(NEW)) {
            n = new Node(t);
            n.push(MemberExpression(t, x, false));
            if (t.match(LEFT_PAREN)) {
                n.type = NEW_WITH_ARGS;
                n.push(ArgumentList(t, x));
            }
        } else {
            n = PrimaryExpression(t, x);
        }

        while ((tt = t.get()) !== END) {
            switch (tt) {
              case DOT:
                n2 = new Node(t);
                n2.push(n);
                n2.push(IdentifierName(t));
                break;

              case LEFT_BRACKET:
                n2 = new Node(t, { type: INDEX });
                n2.push(n);
                n2.push(Expression(t, x));
                t.mustMatch(RIGHT_BRACKET);
                break;

              case LEFT_PAREN:
                if (allowCallSyntax) {
                    n2 = new Node(t, { type: CALL });
                    n2.push(n);
                    n2.push(ArgumentList(t, x));
                    break;
                }

                // FALL THROUGH
              default:
                t.unget();
                return n;
            }

            n = n2;
        }

        return n;
    }

    function ArgumentList(t, x) {
        var n, n2;

        n = new Node(t, { type: LIST });
        if (t.match(RIGHT_PAREN, true))
            return n;
        do {
            n2 = AssignExpression(t, x);
            if (n2.type === YIELD && !n2.parenthesized && t.peek() === COMMA)
                throw t.newSyntaxError("Yield expression must be parenthesized");
            if (t.match(FOR)) {
                n2 = GeneratorExpression(t, x, n2);
                if (n.children.length > 1 || t.peek(true) === COMMA)
                    throw t.newSyntaxError("Generator expression must be parenthesized");
            }
            n.push(n2);
        } while (t.match(COMMA));
        t.mustMatch(RIGHT_PAREN);

        return n;
    }

    function PrimaryExpression(t, x) {
        var n, n2, tt = t.get(true);

        switch (tt) {
          case FUNCTION:
            n = FunctionDefinition(t, x, false, EXPRESSED_FORM);
            break;

          case LEFT_BRACKET:
            n = new Node(t, { type: ARRAY_INIT });
            while ((tt = t.peek(true)) !== RIGHT_BRACKET) {
                if (tt === COMMA) {
                    t.get();
                    n.push(null);
                    continue;
                }
                n.push(AssignExpression(t, x));
                if (tt !== COMMA && !t.match(COMMA))
                    break;
            }

            // If we matched exactly one element and got a FOR, we have an
            // array comprehension.
            if (n.children.length === 1 && t.match(FOR)) {
                n2 = new Node(t, { type: ARRAY_COMP,
                                   expression: n.children[0],
                                   tail: ComprehensionTail(t, x) });
                n = n2;
            }
            t.mustMatch(RIGHT_BRACKET);
            break;

          case LEFT_CURLY:
            var id, fd;
            n = new Node(t, { type: OBJECT_INIT });

          object_init:
            if (!t.match(RIGHT_CURLY)) {
                do {
                    tt = t.get();
                    if ((t.token.value === "get" || t.token.value === "set") &&
                        t.peek() === IDENTIFIER) {
                        if (x.ecma3OnlyMode)
                            throw t.newSyntaxError("Illegal property accessor");
                        n.push(FunctionDefinition(t, x, true, EXPRESSED_FORM));
                    } else {
                        var comments = t.blockComments;
                        switch (tt) {
                          case IDENTIFIER: case NUMBER: case STRING:
                            id = new Node(t, { type: IDENTIFIER });
                            break;
                          case RIGHT_CURLY:
                            if (x.ecma3OnlyMode)
                                throw t.newSyntaxError("Illegal trailing ,");
                            break object_init;
                          default:
                            if (t.token.value in definitions.keywords) {
                                id = new Node(t, { type: IDENTIFIER });
                                break;
                            }
                            throw t.newSyntaxError("Invalid property name");
                        }
                        if (t.match(COLON)) {
                            n2 = new Node(t, { type: PROPERTY_INIT });
                            n2.push(id);
                            n2.push(AssignExpression(t, x));
                            n2.blockComments = comments;
                            n.push(n2);
                        } else {
                            // Support, e.g., |var {x, y} = o| as destructuring shorthand
                            // for |var {x: x, y: y} = o|, per proposed JS2/ES4 for JS1.8.
                            if (t.peek() !== COMMA && t.peek() !== RIGHT_CURLY)
                                throw t.newSyntaxError("missing : after property");
                            n.push(id);
                        }
                    }
                } while (t.match(COMMA));
                t.mustMatch(RIGHT_CURLY);
            }
            break;

          case LEFT_PAREN:
            n = ParenExpression(t, x);
            t.mustMatch(RIGHT_PAREN);
            n.parenthesized = true;
            break;

          case LET:
            n = LetBlock(t, x, false);
            break;

          case NULL: case THIS: case TRUE: case FALSE:
          case IDENTIFIER: case NUMBER: case STRING: case REGEXP:
            n = new Node(t);
            break;

          default:
            throw t.newSyntaxError("missing operand; found " + definitions.tokens[tt]);
            break;
        }

        return n;
    }

    /*
     * parse :: (source, filename, line number) -> node
     */
    function parse(s, f, l) {
        var t = new lexer.Tokenizer(s, f, l);
        var n = Script(t, false, false);
        if (!t.done)
            throw t.newSyntaxError("Syntax error");

        return n;
    }

    /*
     * parseStdin :: (source, {line number}, string, (string) -> boolean) -> program node
     */
    function parseStdin(s, ln, prefix, isCommand) {
        // the special .begin command is only recognized at the beginning
        if (s.match(/^[\s]*\.begin[\s]*$/)) {
            ++ln.value;
            return parseMultiline(ln, prefix);
        }

        // commands at the beginning are treated as the entire input
        if (isCommand(s.trim()))
            s = "";

        for (;;) {
            try {
                var t = new lexer.Tokenizer(s, "stdin", ln.value);
                var n = Script(t, false, false);
                ln.value = t.lineno;
                return n;
            } catch (e) {
                if (!t.unexpectedEOF)
                    throw e;

                // commands in the middle are not treated as part of the input
                var more;
                do {
                    if (prefix)
                        putstr(prefix);
                    more = readline();
                    if (!more)
                        throw e;
                } while (isCommand(more.trim()));

                s += "\n" + more;
            }
        }
    }

    /*
     * parseMultiline :: ({line number}, string | null) -> program node
     */
    function parseMultiline(ln, prefix) {
        var s = "";
        for (;;) {
            if (prefix)
                putstr(prefix);
            var more = readline();
            if (more === null)
                return null;
            // the only command recognized in multiline mode is .end
            if (more.match(/^[\s]*\.end[\s]*$/))
                break;
            s += "\n" + more;
        }
        var t = new lexer.Tokenizer(s, "stdin", ln.value);
        var n = Script(t, false, false);
        ln.value = t.lineno;
        return n;
    }

    return {
        parse: parse,
        parseStdin: parseStdin,
        Node: Node,
        DECLARED_FORM: DECLARED_FORM,
        EXPRESSED_FORM: EXPRESSED_FORM,
        STATEMENT_FORM: STATEMENT_FORM,
        Tokenizer: lexer.Tokenizer,
        FunctionDefinition: FunctionDefinition,
        Module: Module,
        Export: Export
    };

}());
;
/* vim: set sw=4 ts=4 et tw=78: */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is the Narcissus JavaScript engine.
 *
 * The Initial Developer of the Original Code is
 * Brendan Eich <brendan@mozilla.org>.
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Shu-Yu Guo <shu@rfrn.org>
 *   Bruno Jouhier
 *   Gregor Richards
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

/*
 * Narcissus - JS implemented in JS.
 *
 * Decompiler and pretty-printer.
 */

Narcissus.decompiler = (function() {

    const lexer = Narcissus.lexer;
    const parser = Narcissus.parser;
    const definitions = Narcissus.definitions;
    const tokens = definitions.tokens;

    // Set constants in the local scope.
    eval(definitions.consts);

    function indent(n, s) {
        var ss = "", d = true;

        for (var i = 0, j = s.length; i < j; i++) {
            if (d)
                for (var k = 0; k < n; k++)
                    ss += " ";
            ss += s[i];
            d = s[i] === '\n';
        }

        return ss;
    }

    function isBlock(n) {
        return n && (n.type === BLOCK);
    }

    function isNonEmptyBlock(n) {
        return isBlock(n) && n.children.length > 0;
    }

    function nodeStrEscape(str) {
        return str.replace(/\\/g, "\\\\")
                  .replace(/"/g, "\\\"")
                  .replace(/\n/g, "\\n")
                  .replace(/\r/g, "\\r")
                  .replace(/</g, "\\u003C")
                  .replace(/>/g, "\\u003E");
    }

    function nodeStr(n) {
        if (/[\u0000-\u001F\u0080-\uFFFF]/.test(n.value)) {
            // use the convoluted algorithm to avoid broken low/high characters
            var str = "";
            for (var i = 0; i < n.value.length; i++) {
                var c = n.value[i];
                if (c <= "\x1F" || c >= "\x80") {
                    var cc = c.charCodeAt(0).toString(16);
                    while (cc.length < 4) cc = "0" + cc;
                    str += "\\u" + cc;
                } else {
                    str += nodeStrEscape(c);
                }
            }
            return '"' + str + '"';
        }

        return '"' + nodeStrEscape(n.value) + '"';
    }

    function pp(n, d, inLetHead) {
        var topScript = false;

        if (!n)
            return "";
        if (!(n instanceof Object))
            return n;
        if (!d) {
            topScript = true;
            d = 1;
        }

        var p = "";

        if (n.parenthesized)
            p += "(";

        switch (n.type) {
          case FUNCTION:
          case GETTER:
          case SETTER:
            if (n.type === FUNCTION)
                p += "function";
            else if (n.type === GETTER)
                p += "get";
            else
                p += "set";

            p += (n.name ? " " + n.name : "") + "(";
            for (var i = 0, j = n.params.length; i < j; i++)
                p += (i > 0 ? ", " : "") + pp(n.params[i], d);
            p += ") " + pp(n.body, d);
            break;

          case SCRIPT:
          case BLOCK:
            var nc = n.children;
            if (topScript) {
                // No indentation.
                for (var i = 0, j = nc.length; i < j; i++) {
                    if (i > 0)
                        p += "\n";
                    p += pp(nc[i], d);
                    var eoc = p[p.length - 1];
                    if (eoc != ";")
                        p += ";";
                }

                break;
            }

            p += "{";
            if (n.id !== undefined)
                p += " /* " + n.id + " */";
            p += "\n";
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += "\n";
                p += indent(4, pp(nc[i], d));
                var eoc = p[p.length - 1];
                if (eoc != ";")
                    p += ";";
            }
            p += "\n}";
            break;

          case LET_BLOCK:
            p += "let (" + pp(n.variables, d, true) + ") ";
            if (n.expression)
                p += pp(n.expression, d);
            else
                p += pp(n.block, d);
            break;

          case IF:
            p += "if (" + pp(n.condition, d) + ") ";

            var tp = n.thenPart, ep = n.elsePart;
            var b = isBlock(tp) || isBlock(ep);
            if (!b)
                p += "{\n";
            p += (b ? pp(tp, d) : indent(4, pp(tp, d))) + "\n";

            if (ep) {
                if (!b)
                    p += "} else {\n";
                else
                    p += " else ";

                p += (b ? pp(ep, d) : indent(4, pp(ep, d))) + "\n";
            }
            if (!b)
                p += "}";
            break;

          case SWITCH:
            p += "switch (" + pp(n.discriminant, d) + ") {\n";
            for (var i = 0, j = n.cases.length; i < j; i++) {
                var ca = n.cases[i];
                if (ca.type === CASE)
                    p += "  case " + pp(ca.caseLabel, d) + ":\n";
                else
                    p += "  default:\n";
                ps = pp(ca.statements, d);
                p += ps.slice(2, ps.length - 2) + "\n";
            }
            p += "}";
            break;

          case FOR:
            p += "for (" + pp(n.setup, d) + "; "
                         + pp(n.condition, d) + "; "
                         + pp(n.update, d) + ") ";

            var pb = pp(n.body, d);
            if (!isBlock(n.body))
                p += "{\n" + indent(4, pb) + ";\n}";
            else if (n.body)
                p += pb;
            break;

          case WHILE:
            p += "while (" + pp(n.condition, d) + ") ";

            var pb = pp(n.body, d);
            if (!isBlock(n.body))
                p += "{\n" + indent(4, pb) + ";\n}";
            else
                p += pb;
            break;

          case FOR_IN:
            var u = n.varDecl;
            p += n.isEach ? "for each (" : "for (";
            p += (u ? pp(u, d) : pp(n.iterator, d)) + " in " +
                 pp(n.object, d) + ") ";

            var pb = pp(n.body, d);
            if (!isBlock(n.body))
                p += "{\n" + indent(4, pb) + ";\n}";
            else if (n.body)
                p += pb;
            break;

          case DO:
            p += "do " + pp(n.body, d);
            p += " while (" + pp(n.condition, d) + ");";
            break;

          case BREAK:
            p += "break" + (n.label ? " " + n.label : "") + ";";
            break;

          case CONTINUE:
            p += "continue" + (n.label ? " " + n.label : "") + ";";
            break;

          case TRY:
            p += "try ";
            p += pp(n.tryBlock, d);
            for (var i = 0, j = n.catchClauses.length; i < j; i++) {
                var t = n.catchClauses[i];
                p += " catch (" + pp(t.varName, d) +
                                (t.guard ? " if " + pp(t.guard, d) : "") +
                                ") ";
                p += pp(t.block, d);
            }
            if (n.finallyBlock) {
                p += " finally ";
                p += pp(n.finallyBlock, d);
            }
            break;

          case THROW:
            p += "throw " + pp(n.exception, d);
            break;

          case RETURN:
            p += "return";
            if (n.value)
              p += " " + pp(n.value, d);
            break;

          case YIELD:
            p += "yield";
            if (n.value)
              p += " " + pp(n.value, d);
            break;

          case GENERATOR:
            p += pp(n.expression, d) + " " + pp(n.tail, d);
            break;

          case WITH:
            p += "with (" + pp(n.object, d) + ") ";
            p += pp(n.body, d);
            break;

          case LET:
          case VAR:
          case CONST:
            var nc = n.children;
            if (!inLetHead) {
                p += tokens[n.type] + " ";
            }
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += ", ";
                var u = nc[i];
                p += pp(u.name, d);
                if (u.initializer)
                    p += " = " + pp(u.initializer, d);
            }
            break;

          case DEBUGGER:
            p += "debugger NYI\n";
            break;

          case SEMICOLON:
            if (n.expression) {
                p += pp(n.expression, d) + ";";
            }
            break;

          case LABEL:
            p += n.label + ":\n" + pp(n.statement, d);
            break;

          case COMMA:
          case LIST:
            var nc = n.children;
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += ", ";
                p += pp(nc[i], d);
            }
            break;

          case ASSIGN:
            var nc = n.children;
            var t = n.assignOp;
            p += pp(nc[0], d) + " " + (t ? tokens[t] : "") + "="
                              + " " + pp(nc[1], d);
            break;

          case HOOK:
            var nc = n.children;
            p += "(" + pp(nc[0], d) + " ? "
                     + pp(nc[1], d) + " : "
                     + pp(nc[2], d);
            p += ")";
            break;

          case OR:
          case AND:
            var nc = n.children;
            p += "(" + pp(nc[0], d) + " " + tokens[n.type] + " "
                     + pp(nc[1], d);
            p += ")";
            break;

          case BITWISE_OR:
          case BITWISE_XOR:
          case BITWISE_AND:
          case EQ:
          case NE:
          case STRICT_EQ:
          case STRICT_NE:
          case LT:
          case LE:
          case GE:
          case GT:
          case IN:
          case INSTANCEOF:
          case LSH:
          case RSH:
          case URSH:
          case PLUS:
          case MINUS:
          case MUL:
          case DIV:
          case MOD:
            var nc = n.children;
            p += "(" + pp(nc[0], d) + " " + tokens[n.type] + " "
                     + pp(nc[1], d) + ")";
            break;

          case DELETE:
          case VOID:
          case TYPEOF:
            p += tokens[n.type] + " "  + pp(n.children[0], d);
            break;

          case NOT:
          case BITWISE_NOT:
            p += tokens[n.type] + pp(n.children[0], d);
            break;

          case UNARY_PLUS:
            p += "+" + pp(n.children[0], d);
            break;

          case UNARY_MINUS:
            p += "-" + pp(n.children[0], d);
            break;

          case INCREMENT:
          case DECREMENT:
            if (n.postfix) {
                p += pp(n.children[0], d) + tokens[n.type];
            } else {
                p += tokens[n.type] + pp(n.children[0], d);
            }
            break;

          case DOT:
            var nc = n.children;
            p += pp(nc[0], d) + "." + pp(nc[1], d);
            break;

          case INDEX:
            var nc = n.children;
            p += pp(nc[0], d) + "[" + pp(nc[1], d) + "]";
            break;

          case CALL:
            var nc = n.children;
            p += pp(nc[0], d) + "(" + pp(nc[1], d) + ")";
            break;

          case NEW:
          case NEW_WITH_ARGS:
            var nc = n.children;
            p += "new " + pp(nc[0], d);
            if (nc[1])
                p += "(" + pp(nc[1], d) + ")";
            break;

          case ARRAY_INIT:
            p += "[";
            var nc = n.children;
            for (var i = 0, j = nc.length; i < j; i++) {
                if(nc[i])
                    p += pp(nc[i], d);
                p += ","
            }
            p += "]";
            break;

          case ARRAY_COMP:
            p += "[" + pp (n.expression, d) + " ";
            p += pp(n.tail, d);
            p += "]";
            break;

          case COMP_TAIL:
            var nc = n.children;
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += " ";
                p += pp(nc[i], d);
            }
            if (n.guard)
                p += " if (" + pp(n.guard, d) + ")";
            break;

          case OBJECT_INIT:
            var nc = n.children;
            if (nc[0] && nc[0].type === PROPERTY_INIT)
                p += "{\n";
            else
                p += "{";
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0) {
                    p += ",\n";
                }

                var t = nc[i];
                if (t.type === PROPERTY_INIT) {
                    var tc = t.children;
                    var l;
                    /*
                      * See if the left needs to be quoted.
                      *
                      * N.B. If negative numeral prop names ever get converted
                      * internally to numbers by the parser, we need to quote
                      * those also.
                      */
                    var propName = tc[0].value;
                    if (typeof propName === "string" && !lexer.isIdentifier(propName)) {
                        l = nodeStr(tc[0]);
                    } else {
                        l = pp(tc[0], d);
                    }
                    p += indent(4, l) + ": " +
                         indent(4, pp(tc[1], d)).substring(4);
                } else {
                    p += indent(4, pp(t, d));
                }
            }
            p += "\n}";
            break;

          case NULL:
            p += "null";
            break;

          case THIS:
            p += "this";
            break;

          case TRUE:
            p += "true";
            break;

          case FALSE:
            p += "false";
            break;

          case IDENTIFIER:
          case NUMBER:
          case REGEXP:
            p += n.value;
            break;

          case STRING:
            p += nodeStr(n);
            break;

          case GROUP:
            p += "(" + pp(n.children[0], d) + ")";
            break;

          default:
            throw "PANIC: unknown operation " + tokens[n.type] + " " + n.toSource();
        }

        if (n.parenthesized)
            p += ")";

        return p;
    }

    return {
        pp: pp
    };

}());
;
/*
 * Copyright (c) 2011, Intel Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, 
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, 
 *   this list of conditions and the following disclaimer in the documentation 
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

if (RiverTrail === undefined) {
    var RiverTrail = {};
}

RiverTrail.definitions = function () {
    var tokens= [ "CAST", "TOINT32", "FLATTEN" ];
    const offset = Narcissus.definitions.tokens.length;

    var consts = "const ";
    for (var idx = 0; idx < tokens.length; idx++) {
        consts += tokens[idx] + "=" + (offset + idx);
        if (idx < tokens.length - 1) {
            consts += ",";
        }
    }
    consts += ";";

    // add all tokens into a single array
    tokens = Narcissus.definitions.tokens.concat(tokens);

    return {"consts" : consts, "tokens" : tokens};
}();


;
/*
 * Copyright (c) 2011, Intel Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, 
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, 
 *   this list of conditions and the following disclaimer in the documentation 
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

if (RiverTrail === undefined) {
    var RiverTrail = {};
}

RiverTrail.Helper = function () {
    eval(Narcissus.definitions.consts);

    var nodeNames = ["children", "body", "setup", "condition", "update", "thenPart", "elsePart", "expression", "initializer"];

    function traverseAst (ast, f, env) {
        if (ast) {
            ast = f(ast, env);

            for (var field in nodeNames) {
                if (ast[nodeNames[field]]) {
                    if (ast[nodeNames[field]] instanceof Array) {
                        ast[nodeNames[field]] = ast[nodeNames[field]].map(function (x) { return traverseAst(x, f, env); });
                    } else {
                        ast[nodeNames[field]] = traverseAst(ast[nodeNames[field]], f, env);
                    }
                }
            }
        }
        return ast;
    }
            
    function wrappedPP (ast) {
        var s;

        try {
            s = Narcissus.decompiler.pp(ast);
        } catch (e) {
            s = "<no source>";
        }

        return s;
    }

    //
    // Function and helpers to infer the type of a Parallel Array
    //
    // https://cvs.khronos.org/svn/repos/registry/trunk/public/webgl/doc/spec/TypedArray-spec.html
    // gives the following Equivalent C types
    var arrayTypeToCType = [
        [Int8Array, "signed char"],
        [Uint8Array, "unsigned char"],
        [Uint8ClampedArray, "unsigned /* clamped */ char"],
        [Int16Array, "short"],
        [Uint16Array, "unsigend short"],
        [Int32Array, "int"],
        [Uint32Array, "unsigned int"],
        [Float32Array, "float"],
        [Float64Array, "double"]
    ];
    
    function constructorToElementalType (constructor) {
        var i;
        for (i=0;i<arrayTypeToCType.length;i++) {
            if (constructor === arrayTypeToCType[i][0]) {
                return arrayTypeToCType[i][1];
            }
        }
        return undefined;
    };

    function elementalTypeToConstructor (type) {
        var i;
        for (i=0;i<arrayTypeToCType.length;i++) {
            if (type === arrayTypeToCType[i][1]) {
                return arrayTypeToCType[i][0];
            }
        }
        return undefined;
    };

    function inferTypedArrayType(array) {
        var i;
        var elementalType;
        for (i=0;i<arrayTypeToCType.length;i++) {
            if (array instanceof arrayTypeToCType[i][0]) {
                elementalType = arrayTypeToCType[i][1];
                break;
            }
        }
        if (elementalType === undefined) {
            // SAH: I fail here as we do not know the type of this typed array. If it had
            //      a homogeneous type, the constructor would have converted it to a 
            //      typed array.
            throw new TypeError("Cannot infer type for given Parallel Array data container.");
        } 
        return elementalType;
    };

    function inferPAType(pa) {
        var dimSize = pa.getShape();
        var elementalType;
        //
        // if we already have type information, we return it.
        // 
        if (pa.elementalType === undefined) {
            pa.elementalType = inferTypedArrayType(pa.data);
        }
        return {"dimSize": dimSize, "inferredType" : pa.elementalType};
    }; 

    function stripToBaseType(s) {
        const regExp = /([a-zA-Z ]|\/\*|\*\/)*/;
        var match = s.match(regExp);
        return match[0];
    };

    // This is the helper version of TLiteral.prototype.getOpenCLSize()
    // These functions should be in sync.
    // Argument 't' is some scalar or pointer type
    function getOpenCLSize(type) {
        var base_type = stripToBaseType(type);
        if(base_type === type) {
            switch (base_type) {
                case "signed char":
                case "unsigned char":
                case "unsigned /* clamped */ char":
                    return 1;
                    break;
                case "short":
                case "unsigned short":
                    return 2;
                    break;
                case "float":
                case "int":
                case "unsigned int":
                    return 4;
                    break;
                case "double":
                    return 8;
                    break;
                default:
                 reportBug("size of type not known: " + type);
                 break;
            }
        }
        else { // 'type' is a pointer type.
            return 8;
        }
    }
    
    var Integer = function Integer(value) {
        this.value = value;
        return this;
    };

    // Returns a flat copy of a potentially nested JS Array "src"
    // We essentially do a depth first traversal of the nested array structure
    // and copy each Array of scalars encountered to the destination object.
    // This is potentially slower than the _fast implementation below.
    var FlatArray = function FlatArray(constructor, src) {
        var shape = this.shape = new Array();
        var ptr = src; var len = 1;
        var pos = 0;
        while (ptr instanceof Array) {
            shape.push(ptr.length);
            len *= ptr.length;
            ptr = ptr[0];
        }
        var data = this.data = new constructor(len);
        if(shape.length === 1) {
            for(var k = 0; k < shape[0]; k++) {
                if (typeof(src[k]) !== 'number') {
                    throw "Error: Conversion to flat array failed: not a number!";
                }
                this.data[pos++] = src[k];
            }
            return this;
        }
        ptr = src;
        var stack = new Array();
        stack.push(ptr);
        pos = 0;
        while(stack.length !== 0) {
            var node = stack.pop(); 
            if(!(node instanceof Array)) {
                throw "Error: Non array node pushed!! Flattening kernel argument failed.";
            }
            if (node[0] instanceof Array) {
                var len = node[0].length;
                for(var i = node.length-1; i >= 0; i--) {
                    if(!(node[i] instanceof Array) || (node[i].length !== len)) {
                        throw "Error: Invalid array shape !! Flattening kernel argument failed";
                    }
                    stack.push(node[i]);
                }
                continue;
            }
            else {
                if(node.length !== shape[shape.length-1]) {
                    throw "Error: Leaf length and shape are different! Flattening kernel argument failed";
                }
                for(var j = 0; j < node.length; j++) {
                    if (typeof(node[j]) !== 'number') {
                        throw "Error: Conversion to flat array failed: not a number!";
                    }
                    this.data[pos++] = node[j];
                }
            }
        }
        return this;
    }
    var FlatArray_fast = function FlatArray_fast(constructor, src) {
        var shape = this.shape = new Array();
        var ptr = src;
        var len = 1;

        while (ptr instanceof Array) {
            shape.push(ptr.length);
            len *= ptr.length;
            ptr = ptr[0];
        }

        var data = this.data = new constructor(len);
        
        var ptrstack = new Array();
        var pstack = new Array();
        var level = 0;
        var wpos = 0, pos = 0;
        ptr = src;
        
        while (wpos < len) {
            if (ptr[pos] instanceof Array) {
                // check conformity
                if (ptr[pos].length != shape[level+1]) throw "inhomogeneous array encountered";
                // go deeper
                ptrstack[level] = ptr;
                pstack[level] = pos+1;
                ptr = ptr[pos];
                pos = 0;
                level++;
            } else {
                // copy elements. If we get here, first check that we are at the bottom level
                // according to the shape
                if (level != shape.length-1) throw "inhomogeneous array encountered";
                // if this is uniform, we can just copy the rest of this level without 
                // further checking for arrays
                for (; pos < ptr.length; pos++,wpos++) {
                    this.data[wpos] = ptr[pos];
                    if (this.data[wpos] !== ptr[pos]) throw new "conversion error";
                }
            }
            if (pos === ptr.length) {
                // end of level
                level--;
                pos = pstack[level];
                ptr = ptrstack[level];
            }
        }

        return this;
    };

    var compareObjectFields = function(f1, f2) {
        if((f2.hasOwnProperty(idx) && f1[idx].equals(f2[idx]))) {
            return true;
        }
        return false;
    };

    // helper function that throws an exception and logs it if verboseDebug is on
    var debugThrow = function (e) {
        if (RiverTrail.compiler.verboseDebug) {
            console.log("Exception: " + JSON.stringify(e));
        }
        throw e;
    };

    // This is used to check whether an object is a typed array. It is specialized
    // depending on whether the browser supports typed arrays or not.
    var isTypedArray;
    if ((typeof(Float32Array) == "function") && (typeof(Float32Array.prototype) == "object")) {
        isTypedArray = function (arr) {
            return ((arr instanceof Float32Array) || (arr instanceof Float64Array) ||
                    (arr instanceof Int8Array) || (arr instanceof Int16Array) ||
                    (arr instanceof Int32Array) || (arr instanceof Uint8Array) ||
                    (arr instanceof Uint16Array) || (arr instanceof Uint32Array) ||
                    ((typeof(Uint8ClampedArray) == "function") && (arr instanceof Uint8ClampedArray)));
        };
    } else {
        isTypedArray = function( arr) {
            return false;
        }
    }

    // 
    // helper functions for using the narcissus parser to parse a single function. The are used by the
    // driver and by type inference for external references.
    //

    //
    // Name generator to ensure that function names are unique if we parse
    // multiple functions with the same name
    //
    var nameGen = function () {
        var counter = 0;

        return function nameGen (postfix) {
            return "f" + (counter++) + "_" + (postfix || "nameless");
        };
    }();

    //
    // given a string, return a parsed AST
    //
    var parseFunction = function (kernel) {
        var parser = Narcissus.parser;
        var kernelJS = kernel.toString();
        // We want to parse a function that was used in expression position
        // without creating a <script> node around it, nor requiring it to
        // have a name. So we have to take a side entry into narcissus here.
        var t = new parser.Tokenizer(kernelJS);
        t.get(true); // grab the first token
        var ast = parser.FunctionDefinition(t, undefined, false, parser.EXPRESSED_FORM);        
        // Ensure that the function has a valid name to simplify the treatment downstream
        if (!ast.name) ast.name = "nameless";
        return ast;
    };

    //
    // helper to clone the AST for function specialisation. We do not aim to deep clone here, just the 
    // structure of the spine as created by Narcissus. All extra annotations are discarded.
    //
    var cloneAST = function (ast) {
        var funAsString = wrappedPP(ast);
        return parseFunction(funAsString);
    }

    //
    // tree copying --- can copy the AST up until after type inference
    //
    var cloneFunction = function (dropTypes) {
        var copyLut = undefined;
        var varLut = undefined;
        var counter = function () {
                var cnt = 0;
                return function () { return cnt++; };
            }();
        var cntMin = 0;
        var cloneAstArray = function cloneAstArray(array) {
            return array.map(cloneSon);
        };
        var cloneAstFlow = 
            dropTypes ?
            function nothing() { return undefined; } :
            function cloneFlowNode(flow) {
                var result = copyLut[flow.label];
                if (!result) {
                    // ast nodes are fixed up later. everything else is lut copied
                    if (flow instanceof RiverTrail.Typeinference.FFunction) {
                        result = new RiverTrail.Typeinference.FFunction(cloneAstArray(flow.params), cloneAstType(flow.result), flow.root, undefined /* patch up later */); 
                    } else if (flow instanceof RiverTrail.Typeinference.FCall) {
                        // We duplicate the call flow node, but not the function frame it points to, as we do not
                        // copy the called function, either. We need to update the reference counter, though!
                        result = new RiverTrail.Typeinference.FCall(cloneAstArray(flow.params), flow.frame, cloneAstType(flow.result), undefined /* patch up later */);
                        result.frame.uses++;
                    } else if (flow instanceof RiverTrail.Typeinference.FParam) {
                        result = new RiverTrail.Typeinference.FParam(flow.number, cloneAstFlow(flow.call))
                    } else {
                        throw "unknown flow";
                    }

                    copyLut[flow.label] = result;
                }
                    
                return result;
            };
        var cloneAstType = 
            dropTypes ? 
            function nothing() { return undefined; } :
            function cloneAstType(type) {
                var result = copyLut[type.label];
                if (!result) {
                    result = type.clone(copyLut);
                    if (type.flowTo) {
                        result.flowTo = type.flowTo.map(cloneSon);
                    }
                }

                return result;
            };
        var cloneAstNode = function cloneAstNode(ast) {
            if (ast.type === IDENTIFIER) {
                // These nodes may appear twice in the ast, once in varDecls
                // and once in the body. So we need to lut-copy here
                if (ast.cloneLabel && (ast.cloneLabel > cntMin)) {
                    // we have a previous copy
                    return varLut[ast.cloneLabel-cntMin];
                } 
            }
            var result = new Narcissus.parser.Node(ast.tokenizer);
            for (var key in ast) {
                // we hard code a son exclusion list here. Somewhat ugly but probably
                // the fastest solution.
                switch (key) {
                    case "length":
                    case "specStore":
                    case "adrSpecStore":
                    case "redispatched":
                        break;
                    case "funDecls":
                        result[key] = [];
                        break;
                    default:
                        result[key] = cloneSon(ast[key]);
                }
            }
            // some fixup
            if ((result.type === FUNCTION) && result.flowFrame) {
                result.flowFrame.ast = result;
            }
            if ((result.type === CALL) && result.callFrame) {
                result.callFrame.ast = result;
            }
            if (ast.type === IDENTIFIER) {
                // remember this clone
                ast.cloneLabel = counter();
                varLut[ast.cloneLabel-cntMin] = result;
            }

            return result;
        };
        var cloneSon = function cloneSon(son) {
            if (son instanceof Array) {
                return cloneAstArray(son);
            } else if (son instanceof Narcissus.parser.Node) {
                return cloneAstNode(son);
            } else if (son instanceof RiverTrail.Typeinference.Type) {
                return cloneAstType(son);
            } else if (son instanceof RiverTrail.Typeinference.FlowNode) {
                return cloneAstFlow(son);
            } else {
                return son;
            };
        };

        return function (ast) {
            copyLut = [];
            varLut = [];
            cntMin = counter();
            var result = cloneAstNode(ast);
            result.dispatch = nameGen(result.name || (result.name = "nameless"));
            return result;
        };
    };

    //
    // error reporting helper functions
    //
    function reportError(msg, t) {
        throw "Error: " + msg + " [source code was `" + (t ? wrappedPP(t) : "no context") + "`]"; // could be more elaborate
    }
    function reportBug(msg, t) {
        throw "Bug: " + msg; // could be more elaborate
    }

    //
    // helper to follow a selection chain to the root identifier
    //
    function findSelectionRoot(ast) {
        switch (ast.type) {
            case INDEX:
                return findSelectionRoot(ast.children[0]);
                break; // superfluous, I know
            case IDENTIFIER:
                return ast;
                break; // superfluous, I know
            default:
                throw "malformed lhs sel expression in assignment";
        }
    };

    // used in genOCL and infermem to decide whether a return expression qualifies for
    // allocation free copying
    function isArrayLiteral (ast) {
        return ((ast.type === ARRAY_INIT) &&
                ((ast.typeInfo.getOpenCLShape().length == 1) ||
                 ast.children.every(function (x) { return (x.type === IDENTIFIER) || isArrayLiteral(x);})));
    };

    // allocate an aligned Typed Array
    function allocateAlignedTA(template, length) {
        if(!RiverTrail.compiler){
            return new template(length);
        }
        var alignment = RiverTrail.compiler.openCLContext.alignmentSize;
        if (!alignment) {
            // old extension, do not align
            return undefined;
            return new constructor(size);
        }
        var buffer = new ArrayBuffer(length * template.BYTES_PER_ELEMENT + alignment);
        var offset = RiverTrail.compiler.openCLContext.getAlignmentOffset(buffer);
        return new template(buffer, offset, length);
    };

    return { "traverseAst" : traverseAst,
             "wrappedPP" : wrappedPP,
             "inferPAType" : inferPAType,
             "elementalTypeToConstructor" : elementalTypeToConstructor,
             "stripToBaseType" : stripToBaseType,
             "getOpenCLSize" : getOpenCLSize,
             "Integer" : Integer,
             "FlatArray" : FlatArray,
             "debugThrow" : debugThrow,
             "isTypedArray" : isTypedArray,
             "inferTypedArrayType" : inferTypedArrayType,
             "cloneFunction" : cloneFunction,
             "nameGen" : nameGen,
             "parseFunction" : parseFunction,
             "reportError" : reportError,
             "reportBug" : reportBug,
             "findSelectionRoot" : findSelectionRoot,
             "isArrayLiteral" : isArrayLiteral,
             "compareObjectFields" : compareObjectFields,
             "allocateAlignedTA" : allocateAlignedTA
    };

}();
;
/**
   Copyright (c) 2013 Adobe Systems Incorporated. All rights reserved.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
   
   http://www.apache.org/licenses/LICENSE-2.0
   
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


 * Copyright (c) 2011, Intel Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, 
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, 
 *   this list of conditions and the following disclaimer in the documentation 
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */



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

})();;
/*

  Copyright (c) 2013 Adobe Systems Incorporated. All rights reserved.
  
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at
  
  http://www.apache.org/licenses/LICENSE-2.0
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  

 * Copyright (c) 2011, Intel Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, 
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, 
 *   this list of conditions and the following disclaimer in the documentation 
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

//
// Type inference phase
//


var globalInlineObjectTypes = [];
ShaderDSL.Typeinference = function () {
    var stackTrace = [];

    eval(Narcissus.definitions.consts);
    eval(RiverTrail.definitions.consts);

    var inferPAType = RiverTrail.Helper.inferPAType;
    var nameGen = RiverTrail.Helper.nameGen;
    var parseGladderArrayDecl = ShaderDSL.parseGladderArrayDecl;
    var isGlMatrixMatSingleton = ShaderDSL.isGlMatrixMatSingleton;
    
    const debug = false;
    //const allowGlobalFuns = false; // Set to true so kernel functions can call global functions.
    const allowGlobalFuns = true; // Set to true so kernel functions can call global functions.
    const lazyJSArrayCheck = true; // check for homogeneity of JS Arrays at runtime


    //
    // error reporting
    //
    function reportError(msg, ast) {
        msg = stackTrace.reduce(function (p, c) {
                        return p + " -> " + c.fun.name + " [call was: " + RiverTrail.Helper.wrappedPP(c.ast) + "]";
                    }, "In main") + ": " + msg;

        RiverTrail.Helper.reportError(msg, ast);
    }
    var reportBug = RiverTrail.Helper.reportBug;

    // 
    // tree copying
    //
    var cloneFunctionNoTypes = RiverTrail.Helper.cloneFunction(true);
    var cloneFunction = RiverTrail.Helper.cloneFunction(false);

    //
    // unique label generator
    //
    var labelGen = function () {
        var cnt = 0;
        
        return function () { return cnt++; };
    }();

    //
    //
    // Base prototype shared by all type structures
    //
    var Type = function (kind) {
        this.kind = kind;
        this.label = labelGen();
    };
    Type.OBJECT = "OBJECT";
    Type.LITERAL = "LITERAL";
    Type.FUNCTION = "FUNCTION";
    Type.BOTTOM = "BOTTOM";

    var Tp = Type.prototype;
    Tp.toString = function () { return "<general type>"; };
    Tp.equals = function(other) {
        return (this.kind === other.kind);
    };
    Tp.isArithType = function () { // type is allowed argument to arithmetic operations
        return ((this.kind === Type.LITERAL) &&
                ((this.type === TLiteral.NUMBER) ||
                 (this.type === TLiteral.BOOL)));
    };
    Tp.isNumberType = function () { // type is allowed argument to arithmetic operations
        return ((this.kind === Type.LITERAL) &&
                (this.type === TLiteral.NUMBER));
    };
    Tp.isTruthType = function () { // type is allowed in predicate positions
        return ((this.kind === Type.LITERAL) &&
                ((this.type === TLiteral.NUMBER) ||
                 (this.type === TLiteral.BOOL)));
    };
    Tp.isBoolType = function () { // type is allowed argument to arithmetic operations
        return ((this.kind === Type.LITERAL) &&
                (this.type === TLiteral.BOOL));
    };
    Tp.isScalarType = function () { // type is a scalar value
        return ((this.kind === Type.LITERAL) &&
                ((this.type === TLiteral.BOOL) || (this.type === TLiteral.NUMBER)));
    };
    Tp.isObjectType = function (name) { // checks whether type is object; name is optional
        return ((this.kind === Type.OBJECT) &&
                ((name === undefined) ||
                 (this.name === name)));
    };
    Tp.isArrayishType = function () { // checks whether the type is an array like type
        return this.isObjectType(TObject.ARRAY) || this.isObjectType(TObject.JSARRAY) || this.isObjectType(TObject.PARALLELARRAY);
    };
    Tp.isSlangArray = function() {
        // JS arrays map onto GLSL arrays
        return this.isObjectType(TObject.JSARRAY);
    }
    Tp.isBottomType = function () {
        return (this.kind === Type.BOTTOM);
    };
    Tp.registerFlow = function (from) {
        (from.flowTo || (from.flowTo = [])).push(this);
    };
    Tp.registerParamFlow = function (param) {
        (this.flowTo || (this.flowTo = [])).push(param);
    };
    Tp.getOpenCLShape = function () {
        return []; // everything is a scalar unless defined otherwise
    };
    Tp.getOpenCLSize = function () {
        reportBug("size of type not known:" + this.kind);
    };
    Tp.getOpenCLAddressSpace = function () {
        return "";
    };
    Tp.hasAddressSpace = function () {
        return (this.getAddressSpace() !== undefined);
    };
    Tp.getAddressSpace = function () {
        return undefined;
    };
    Tp.setAddressSpace = function (val) {
        return;
    };
    Tp.__defineGetter__("slangType", function () {
        return this.OpenCLType;
    });

    Tp.__defineSetter__("slangType", function (name) {
        this.OpenCLType = name;
    });

    Tp.slangTypedName = function (name) {
        if (this.OpenCLType === undefined) debugger;
        return this.OpenCLType + " " + name;
    };
    Tp.isPseudoType = function() { 
        return false;
    }
         

    //
    // literal type for all literals
    //
    var TLiteral = function (type) {
        this.type = type;
        switch (type) {
            case TLiteral.NUMBER:
                this.slangType = "float";
                break;
            case TLiteral.BOOL:
                this.slangType = "bool";
                break;
            case TLiteral.SAMPLER2D:
                this.slangType = "sampler2D";
                break;
            case TLiteral.STRING:
                reportBug("strings unsupported");
                break;
            default:
                reportBug("unknown type for literal " + type);
        }
        this.label = labelGen();
    };
    TLiteral.NUMBER = "NUMBER";
    TLiteral.STRING = "STRING";
    TLiteral.BOOL = "BOOL";
    TLiteral.SAMPLER2D = "sampler2D";


    var TLp = TLiteral.prototype = new Type(Type.LITERAL);
    TLp.toString = function () { return "Literal: " + this.type + "<" + this.slangType + ">"};
    TLp.equals = function (other) {
        return (this.constructor.prototype.equals.call(this, other) &&
                (this.type === other.type));
    };
    TLp.clone = function (lut) {
        var result;
        if (lut && (result = lut[this.label])) {
           return result;
        }
        result = new TLiteral(this.type);
        result.slangType = this.slangType;

        lut && (lut[this.label] = result);
        return result;
    };
    TLp.getOpenCLSize = function getOpenCLSize() {
        switch (this.OpenCLType) {
            case "signed char":
            case "unsigned char":
            case "unsigned /* clamped */ char":
                return 1;
                break;
            case "short":
            case "unsigned short":
                return 2;
                break;
            case "float":
            case "int":
            case "unsigned int":
                return 4;
                break;
            case "double":
                return 8;
                break;
            default:
                reportBug("size of type not known:" + this.OpenCLType);
                break;
        }
    };

    //
    // Function or arrow type. The result is a single type whereas
    // parameters is an array of types.
    //
    var TFunction = function (parameters, result) {
        this.parameters = parameters;
        this.result = result;
        this.label = labelGen();
    };

    var TFp = TFunction.prototype = new Type(Type.FUNCTION);
    TFp.toString = function () { 
        var s = "(";
        for (var pos = 0; pos < this.parameters.length; pos++) {
            s = s + (pos > 0 ? ", " : "") + this.parameters[pos].toString();
        }
        s = s + ") -> " + this.result.toString();
        return s;
    };
    TFp.equals = function (other, argsOnly) {
        return (this.constructor.prototype.equals.call(this, other) &&
                (argsOnly || this.result.equals(other.result)) &&
                (this.parameters.length === other.parameters.length) &&
                this.parameters.every( function (oneP, index) { return oneP.equals(other.parameters[index]);}));
    };
    TFp.clone = function (lut) {
        var result;
        if (lut && (result = lut[this.label])) {
            return result;
        }
        result = new TFunction(this.parameters, this.result);
        lut && (lut[this.label] = result);

        result.parameters = result.parameters.map(function (v) { return v.clone(lut); });
        result.result = result.result.clone(lut);

        return result;
    };

    //
    // Object type. The name is the globally unique name of the object,
    // usually the name of the constructor (e.g. Array, ParallelArray).
    //
    var TObject = function (name) {
        this.name = name;
        this.properties = {};
        this.properties.__proto__ = null;
        this.label = labelGen();
    };
    TObject.ARRAY = "Array";
    TObject.JSARRAY = "JSArray";
    TObject.PARALLELARRAY = "ParallelArray";
    TObject.INLINEOBJECT = "InlineObject";
    TObject.VEC2 = "vec2";
    TObject.VEC3 = "vec3";
    TObject.VEC4 = "vec4";
    TObject.MAT2 = "mat2";
    TObject.MAT3 = "mat3";
    TObject.MAT4 = "mat4";
    TObject.TEXTURE2D = "texture2D";


    TObject.ATTRIBUTES = "Attributes";
    TObject.UNIFORMS = "Uniforms";
    TObject.VARYINGS = "Varyings";


    TObject.makeType = function (typeName, val) {
        switch (typeName) {
        case "int": // losing precision here ?
        case "float":
            return new TLiteral(TLiteral.NUMBER); 
        case "bool":
            return new TLiteral(TLiteral.BOOL);
        case "sampler2D":
            return new TLiteral(TLiteral.SAMPLER2D);
        }
        var handler = this.prototype.registry[typeName];
        if (handler) {
            return handler.makeType(val);
        } else {
            var match = parseGladderArrayDecl(typeName);
            if (match) {
                // FIXME generalize
                return this.prototype.registry["vec" + match.rank].makeType().deriveArrayType();
            } else {
                console.log(typeName);
                debugger;
            }
        }
    };

    function makeTVecN(n)
    {
        var type = new TObject(TObject.PARALLELARRAY);
        type.properties.shape = [n];
        type.properties.elements = new TLiteral(TLiteral.NUMBER);
        type.slangType = "vec" + n;
        return type;
    }

    function makeTMatN(n)
    {
        var type = new TObject(TObject.PARALLELARRAY);
        type.properties.shape = [n, n];
        type.properties.elements = new TLiteral(TLiteral.NUMBER);
        type.slangType = "mat" + n;
        return type;
    }

    TObject.deriveObjectType = function (obj) {
        var name, key;
        var isInstance = function isInstance (x) { 
            return (obj instanceof x);
        };
        for (key in this.prototype.registry) {
            if (((this.prototype.registry[key].constructor !== undefined) &&
                 (obj instanceof this.prototype.registry[key].constructor)) ||
                ((this.prototype.registry[key].constructors !== undefined) &&
                 this.prototype.registry[key].constructors.some(isInstance))) {
                name = key;
                break;
            }
        }
        return name;
    };

    var TOp = TObject.prototype = new Type(Type.OBJECT);

    TOp.registry = {};              // mapping from object names to the
    TOp.registry.__proto__ = null;  // handler that contains implementations
                                    // for abstract interpretation of the 
                                    // object's methods

    TOp.toString = function () { 
        var s = "Object: " + this.name + "[";
        for (var key in this.properties) {
           s = s + key + ":" + (this.properties[key] ? this.properties[key].toString() : "undefined") + ", ";
        }
        s = s + "]";
        s = s + "<" + this.slangType + ">";
        return s;
    };
    TOp.equals = function (other) {
        return (this.constructor.prototype.equals.call(this, other) &&
                (this.name === other.name) &&
                (this.registry[this.name].equals.call(this, other)));
    };
    TOp.clone = function (lut) {
        var result;
        if (lut && (result = lut[this.label])) {
            return result;
        }
        result = new TObject(this.name);

        lut && (lut[this.label] = result);

        if(this.properties.fields) {
            result.properties.fields = {};
            for (var i in this.properties.fields) {
                result.properties.fields[i] = this.properties.fields[i].clone(lut);
            }
        }
        else if (this.properties.typeNames) {
            result.properties.typeNames = {};
            for (var i in this.properties.typeNames) {
                result.properties.typeNames[i] = this.properties.typeNames[i];
            }
        }
        else if (this.properties.elements) {
                result.properties.elements = this.properties.elements.clone(lut);
        }
        result.properties.shape = this.properties.shape;
        result.slangType = this.slangType;


        //result.properties.addressSpace = this.properties.addressSpace;

        return result;
    };
     TOp.isPseudoType = function() {
        var pseudo = this.getHandler().isPseudoType;
        if (pseudo === undefined) return false;
        return pseudo;
    };
    TOp.updateOpenCLType = function () {
        this.getHandler().updateOpenCLType.call(this);
    };
    TOp.getHandler = function () {
        return this.registry[this.name] || reportBug("No object handler for class `" + this.name + "`");
    };
    TOp.getOpenCLShape = function () {
        return this.getHandler().getOpenCLShape.call(this) || [];
    };
    TOp.getOpenCLSize = function () {
        return this.getHandler().getOpenCLSize.call(this) || reportBug("unknown OpenCL size for object: " + this.name);
    };
    TOp.getOpenCLAddressSpace = function () {
        return this.properties.addressSpace || "";
    }
    TOp.getAddressSpace = function () {
        return this.properties.addressSpace;
    };
    TOp.setAddressSpace = function (val) {
        if (this.registry[this.name].setAddressSpace) {
            return this.registry[this.name].setAddressSpace.call(this, val);
        } else {
            this.properties.adressSpace = val;
        }
    }
    TOp.slangTypedName = function(name) {
        return (this.getHandler().slangTypedName || Type.prototype.slangTypedName).call(this, name);
    }

    TOp.deriveArrayType = function(len) {
        var type = new TObject(TObject.JSARRAY);
        type.properties.shape = [len];
        type.properties.elements = this;
        return type;
    }


    //
    // Bottom type for error states
    //
    var TBottom = function () { 
        this.label = labelGen();
    };
    var TBp = TBottom.prototype = new Type(Type.BOTTOM);
    TBp.equals = function (other) { return false; };

    // 
    // type environment AKA symbol table
    //
    var TEnv = function (env, functionFrame) {
        this.parent = env;
        this.bindings = {};
        this.bindings.__proto__ = null;
        this._accu = null;
        if (functionFrame) {
            this._functionResult = null;
            this._roots = [];
        }
    };
    var TEp = TEnv.prototype;
    TEp.lookup = function (name) {
        return (this.bindings[name] !== undefined) ? this.bindings[name] : ((this.parent && this.parent.lookup(name)) || undefined);
    }
    TEp.getType = function (name) {
        var entry = this.lookup(name);
        if (entry) {
            return entry.type;
        } else {
            return undefined;
        }
    };
    TEp.bind = function (name, duplicates, constinit) {
        if (name instanceof Array) {
            name.forEach( this.bind);
        } else {
            if (!duplicates && this.bindings[name] !== undefined) {
                debug && console.log("variable bound twice in single scope: " + name);
            } else {
                this.bindings[name] = {initialized : false, type : null, constinit: constinit };
            }
        }
    }
    TEp.update = function (name, type, constinit) {
        var current = this.lookup(name);
        if (current === undefined) {
            reportError("variable " + name + " has not been previously declared!");
        } else if (current.type === null) {
            var newT = type.clone();
            newT.registerFlow(type);
            this.bindings[name] = {initialized : true, type : newT, constinit: constinit}; // force a new entry in the dataflow graph
        } else if (!current.type.equals(type)) {
            reportError("variable " + name + " is polymorphic: " + current.type.toString() + "/" + type.toString());
        } else {
            // propagate flow information
            current.type.registerFlow(type);
        }
    }
    TEp.intersect = function (other) {
        for (var name in this.bindings) {
            var mType = this.bindings[name];
            var oType = other.bindings[name];
            if (oType === undefined) {
                this.bindings[name].initialized = false;
            } else {
                mType.type.equals(oType.type) || reportError("variable " + name + " is polymorphic: " + mType.type.toString() + "/" + oType.type.toString());
                mType.type.registerFlow(oType.type);
            }
        }
        for (var name in other.bindings) {
            if (this.bindings[name] === undefined) {
                this.bindings[name] = {initialized : false, type : other.bindings[name].type, constinit: other.bindings[name].constinit};
                this.bindings[name].type.registerFlow(other.bindings[name].type);
            }
        }
    };
    TEp.merge = function (other) {
        for (var name in other.bindings) {
            var oType = other.bindings[name];
            this.bindings[name] = {initialized : oType.initialized, type: oType.type.clone(), constinit:oType.constinit};
            this.bindings[name].type.registerFlow(oType.type);
        }
    };
    TEp.tagAllUnitialized = function () {
        for (var name in this.bindings) {
            this.bindings[name].initialized = false;
        }
    };
    TEp.toString = function () {
        var s = "";
        for (var name in this.bindings) {
            s = s + ((s === "") ? "" : ", ") + name + " => " + this.bindings[name].type.toString();
        }
        return "{{" + s + "}}";
    };

    // this construction eases debugging :-)
    TEp.__defineGetter__("accu", function () {
                return this._accu || null;
            });
    TEp.__defineSetter__("accu", function (val) { 
                this._accu = val; 
            });
    TEp.resetAccu = function () {
        this._accu = null;
    }

    // the function result is bubbled up to the first function frame that contains the
    // current frame; this is required to ensure that results from nested script scopes
    // are attributed to the correct function
    TEp.__defineGetter__("functionResult", function () {
                if (this._functionResult !== undefined) {
                    return this._functionResult;
                } else {
                    return parent.functionResult;
                }
            });
    TEp.__defineSetter__("functionResult", function (val) {
                if (val) {
                    if (this._functionResult === undefined) { // this frame does not belong to a function; bubble up
                        this.parent.functionResult = val;
                    } else {
                        if (this._functionResult === null) {
                            this._functionResult = val.clone();
                            this._functionResult.registerFlow(val);
                        } else {
                            this._functionResult.equals(val) || reportError("function has polymorphic return type");
                            this._functionResult.registerFlow(val);
                        }
                    }
                }
            });
    // roots encode the roots of the flow graph on types. It should only ever be defined for the top-level function
    // scope.
    TEp.addRoot = function (val) {
        if (this._roots) {
            this._roots.push(val);
        } else {
            return this.parent.addRoot(val);
        }
    };
    TEp.getRoots = function () {
        if (this._roots) {
            return this._roots;
        } else {
            return this.parent.getRoots();
        }
    };

    //
    // Root environment which models the accesible global scope
    //
    var rootEnvironment = new TEnv();

    [ "Math", TObject.TEXTURE2D,
        TObject.VEC2, TObject.VEC3, TObject.VEC4, TObject.MAT3, TObject.MAT4].forEach(function(n) {
        rootEnvironment.bind(n);
        rootEnvironment.update(n, new TObject(n));
    });
        

    rootEnvironment.bind("gl_Position");
    rootEnvironment.update("gl_Position", makeTVecN(4));

    rootEnvironment.bind("gl_FragColor");
    rootEnvironment.update("gl_FragColor", makeTVecN(4)); 

    // 
    // Handlers for built in classes
    //
    TOp.registry["Math"] = {
        methodCall : function (thisType, name, tEnv, fEnv, ast) {
            var type;
            // grab argument types first
            ast.children[1] = drive(ast.children[1], tEnv, fEnv);
            var argtypes = tEnv.accu;
            tEnv.resetAccu();

            switch (name) {
                case "abs":
                case "acos":
                case "asin":
                case "atan":
                case "ceil":
                case "cos":
                case "exp":
                case "floor":
                case "log":
                case "round":
                case "sin":
                case "sqrt":
                case "tan":
                    // number -> number functions
                    argtypes.length === 1 || reportError("too many arguments for Math." + name, ast);
                    argtypes[0].isArithType() || reportError("argument to Math." + name + " is not a number (found " + argtypes[0].toString() + ")", ast);
                    type = new TLiteral(TLiteral.NUMBER);
                    break;

                case "atan2":
                case "pow":
                case "mod": // FIXME no such method in JS
                    // number, number -> number functions
                    argtypes.length === 2 || reportError("too many arguments for Math." + name, ast);
                    argtypes[0].isArithType() || reportError("first argument to Math." + name + " is not a number (found " + argtypes[0].toString() + ")", ast);
                    argtypes[1].isArithType() || reportError("second argument to Math." + name + " is not a number (found " + argtypes[1].toString() + ")", ast);
                    type = new TLiteral(TLiteral.NUMBER);
                    break;

                case "max":
                case "min":
                    // number, ..., number -> number
                    argtypes.forEach( function (t, idx) { t.isArithType() || reportError("argument " + (idx + 1) + 
                                                                                         " to Math." + name + " is not " +
                                                                                         "a number", ast); });
                    type = new TLiteral(TLiteral.NUMBER);
                    break;

				case "random":
					argtypes.length === 0 || reportError("too many arguments for Math." + name, ast);
                    type = new TLiteral(TLiteral.NUMBER);
				    break;

                default:
                    reportError("Method `" + name + "` of global Math object not supported", ast);
            }

            return type;
        },
        propertySelection : function (name, tEnv, fEnv, ast) {
            var type;

            switch (name) {
                case "E":
                case "LN2":
                case "LN10":
                case "LOG2E":
                case "PI":
                case "SQRT1_2":
                case "SQRT2":
                    type = new TLiteral(TLiteral.NUMBER);
                    break;
                    
                default:
                    reportError("unknown property `Math." + name + "`", ast);
            }

            return type;
        },
        constructor : undefined,
        makeType : null,
        updateOpenCLType : null,
        equals : function (other) {
            return other.name == this.name;
        }
    };

    TOp.registry["InlineObject"] = {
        methodCall : function(thisType, name, tEnv, fEnv, ast) {
            reportError("Methods not supported on Objects");
        },
        propertySelection : function (name, tEnv, fEnv, ast) {
            var type = null;
            var fields = ast.children[0].typeInfo.properties.fields;
            for(var idx in fields) {
                if(name === idx) {
                    return fields[idx];
                }
            }
            reportError("Could not find property", name, "in Object");
        },
        makeType : function(val) {
            var type = new TObject(TObject.INLINEOBJECT);
            type.updateOpenCLType();
            return type;
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
        },
        setAddressSpace : function (val) {
            //this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        equals : function (other) {
            if(other.kind !== "OBJECT" || other.name !== "InlineObject")
                return false;
            var fields = this.properties.fields;
            var other_fields = other.properties.fields;
            if(other_fields === undefined)  return false;
            // other_fields should have exactly the properties which
            // are also in fields, no more, no less. Order doesn't matter since
            // we don't allow "f2 = &(a.f1)+sizeof(f1)" type property addressing.
            if(other_fields.length !== fields.length) return false;
            for(var idx in fields) {
                //if(!fields.hasOwnProperty(idx))
                //    return false;
                if(!other_fields.hasOwnProperty(idx))
                    return false;
                if(!fields[idx].equals(other_fields[idx]))
                    return false;
                /*
                if(fields[idx] === other_fields[idx])
                    continue;
                if(fields[idx].OpenCLType !== other_fields[idx].OpenCLType)
                    return false;
                // The following only compares the outer shape!
                // Fix this to compare the entire shape array
                if(fields[idx].properties.shape !== other_fields[idx].properties.shape)
                    return false;
                */
            }
            return true;
        }
    };

    const NO_MATCHING_SIGNATURE = -1;
    const NO_MATCHING_ARITY = -2;
    function checkArgtypes(formalVariants, actual) { 
        // check lengths first
        var success = false;
        for (var k = 0; k < formalVariants.length; k++) {
            if (actual.length === formalVariants[k].argtypes.length) {
                success = true;
            }
        }
        if (success == false) { 
            return NO_MATCHING_ARITY; // means lengths don't match
        }
        // check types
        for (var k = 0; k < formalVariants.length; k++) {
            var failure = false;
            var formal = formalVariants[k].argtypes;
            for (var i = 0; i < formal.length; i ++)  {
                if (actual[i] === undefined) {
                    if (formalVariants.length == 1) return i; 
                    failure = true;
                    break;
                }
                if (actual[i].isArithType() !== (formal[i] === "float"))  {
                    if (formalVariants.length == 1) return i; 
                    failure = true;
                    break;
                }
                if (actual[i].slangType !== formal[i]) {
                    if (formalVariants.length == 1) { 
                        return i; 
                    }
                    failure = true;
                    break;

                }
                // FIXME all the rest of type testing
            }
            if (failure == false)
                return formalVariants[k]; // got so far without an issue: report the matching signature
        }
        return NO_MATCHING_SIGNATURE; // Means no overloaded signature found.
    }

    function glMatrixSingletonMethodCall(thisType, name, tEnv, fEnv, ast) {
        // grab argument types first
        ast.children[1] = drive(ast.children[1], tEnv, fEnv);
        var argtypes = tEnv.accu;
        tEnv.resetAccu();
        if (this.signatures[name] === undefined) 
            reportError("no known builtin " + name + ", receiver " + thisType, ast);
        var checkResult = checkArgtypes(this.signatures[name], argtypes);
        if (typeof(checkResult) === "number") {
            if (checkResult === NO_MATCHING_ARITY)
                reportError("wrong number of arguments for " + name, ast);
            else if (checkResult == NO_MATCHING_SIGNATURE) 
                reportError("no matching overloaded form found for " + name, ast);
            else 
                reportError("wrong type of argument " + checkResult + " to " + name + " (found " + argtypes[checkResult] + ")", ast);
        }
        return TObject.makeType(checkResult.ret);
    }

    TOp.registry["vec2"] =  {
        signatures: {
            'create':     [{  argtypes: [ ] , ret: TObject.VEC2}], // array of forms
            'createFrom': [{  argtypes: [ "float", "float"], ret: TObject.VEC2 }],
            
            'add':        [{  argtypes: [ TObject.VEC2, TObject.VEC2, TObject.VEC2], ret: TObject.VEC2 }],
            'subtract':   [{  argtypes: [ TObject.VEC2, TObject.VEC2, TObject.VEC2], ret: TObject.VEC2 }],
            'multiply':   [{  argtypes: [ TObject.VEC2, TObject.VEC2, TObject.VEC2], ret: TObject.VEC2 }],
            'divide':     [{  argtypes: [ TObject.VEC2, TObject.VEC2, TObject.VEC2], ret: TObject.VEC2 }],
            'reflect':    [{  argtypes: [ TObject.VEC2, TObject.VEC2, TObject.VEC2], ret: TObject.VEC2 }],
            
            'normalize':  [{  argtypes: [ TObject.VEC2, TObject.VEC2], ret: TObject.VEC2 }],
            'negate':     [{  argtypes: [ TObject.VEC2, TObject.VEC2], ret: TObject.VEC2 }],

            'dot':        [{  argtypes: [ TObject.VEC2, TObject.VEC2], ret: "float" }],

        },

        methodCall : glMatrixSingletonMethodCall,

        propertySelection : function (name, tEnv, fEnv, ast) {
            var fields = ast.children[0].typeInfo.properties.fields;
            for (var idx in fields) {
                if(name === idx) {
                    return fields[idx];
                }
            }
            reportError("Could not find property", name, "in Object");
        },
        makeType : function() {
            return makeTVecN(2);
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
            this.OpenCLType = "vec2";
        },
        setAddressSpace : function (val) {
            //this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        equals : function (other) {
            return other.name === this.name;
        }
    };


    TOp.registry["vec3"] = {

        signatures: {
            'create': [{  argtypes: [ ], ret: TObject.VEC3 }, 
                       {  argtypes: [ TObject.VEC3], ret: TObject.VEC3 },
                       {  argtypes: [ TObject.VEC4], ret: TObject.VEC3 } ], // array of forms
            'createFrom': [{  argtypes: [ "float", "float", "float"], ret: TObject.VEC3 }],
            
            'add':        [{  argtypes: [ TObject.VEC3, TObject.VEC3, TObject.VEC3], ret: TObject.VEC3 }],
            'subtract':   [{  argtypes: [ TObject.VEC3, TObject.VEC3, TObject.VEC3], ret: TObject.VEC3 }],
            'multiply':   [{  argtypes: [ TObject.VEC3, TObject.VEC3, TObject.VEC3], ret: TObject.VEC3 }],
            'divide':     [{  argtypes: [ TObject.VEC3, TObject.VEC3, TObject.VEC3], ret: TObject.VEC3 }],
            'reflect':    [{  argtypes: [ TObject.VEC3, TObject.VEC3, TObject.VEC3], ret: TObject.VEC3 }],
            
            'normalize':  [{  argtypes: [ TObject.VEC3, TObject.VEC3], ret: TObject.VEC3 }],
            'negate':     [{  argtypes: [ TObject.VEC3, TObject.VEC3], ret: TObject.VEC3 }],

            'dot':        [{  argtypes: [ TObject.VEC3, TObject.VEC3], ret: "float" }],
        },

        methodCall : glMatrixSingletonMethodCall,

        propertySelection : function (name, tEnv, fEnv, ast) {
            var type = null;
            var fields = ast.children[0].typeInfo.properties.fields;
            for(var idx in fields) {
                if (name === idx) {
                    return fields[idx];
                }
            }
            reportError("Could not find property", name, "in Object");
        },
        makeType : function() {
            return makeTVecN(3);
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
            this.OpenCLType = TObject.VEC3;
        },
        setAddressSpace : function (val) {
            //this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        equals : function (other) {
            return other.name === this.name;
        }
    };

    TOp.registry["vec4"] = {

        signatures: {
            'create':     [{ argtypes: [ ], ret: TObject.VEC4 }], // array of forms
            'createFrom': [{ argtypes: [ "float", "float", "float", "float"], ret: TObject.VEC4 }],
            
            'add':        [{ argtypes: [ TObject.VEC4, TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 }],
            'subtract':   [{ argtypes: [ TObject.VEC4, TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 }],
            'multiply':   [{ argtypes: [ TObject.VEC4, TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 },
                           { argtypes: [ TObject.MAT4, TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 }],
            'divide':     [{ argtypes: [ TObject.VEC4, TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 }],
            'reflect':    [{ argtypes: [ TObject.VEC4, TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 }],
            
            'normalize':  [{ argtypes: [ TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 }],
            'negate':     [{ argtypes: [ TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 }],

            'dot':        [{ argtypes: [ TObject.VEC4, TObject.VEC4], ret: "float" }],
        },

        methodCall : glMatrixSingletonMethodCall,

        propertySelection : function (name, tEnv, fEnv, ast) {
            var type;
            switch (name) {
                default:
                reportError("unknown property `vec4." + name + "`", ast);
            }
            return type;
        },

        makeType : function() {
            return makeTVecN(4);
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
            this.OpenCLType = TObject.VEC4;
        },
        setAddressSpace : function (val) {
            //this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        equals : function (other) {
            return other.name === this.name;
        }
    };

    TOp.registry["mat4"] = {
        signatures: {
            'create':     [{ argtypes: [ ], ret: TObject.MAT4 }], // array of forms
            'createFrom': [{ argtypes: [ "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float"], ret: TObject.MAT4 }],
            
            'add':        [{ argtypes: [ TObject.MAT4, TObject.MAT4, TObject.MAT4], ret: TObject.MAT4 }],
            'subtract':   [{ argtypes: [ TObject.MAT4, TObject.MAT4, TObject.MAT4], ret: TObject.MAT4 }],
            'normalize':  [{ argtypes: [ TObject.MAT4, TObject.MAT4, TObject.MAT4], ret: TObject.MAT4 }],
            'multiply':   [ { argtypes: [ TObject.MAT4, TObject.VEC4, TObject.VEC4], ret: TObject.VEC4 },
                            { argtypes: [ TObject.MAT4, TObject.MAT4, TObject.MAT4], ret: TObject.MAT4 }],
        },

        methodCall : glMatrixSingletonMethodCall,

        propertySelection : function (name, tEnv, fEnv, ast) {
            var type = null;
            var fields = ast.children[0].typeInfo.properties.fields;
            for (var idx in fields) {
                if (name === idx) {
                    return fields[idx];
                }
            }
            reportError("Could not find property", name, "in mat4");
        },
        makeType : function() {
            return makeTMatN(4);
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
            this.OpenCLType = "mat4";
        },
        setAddressSpace : function (val) {
            //this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        equals : function (other) {
            return other.name == this.name;
        }
    };

    TOp.registry["texture2D"] = {
        signatures: {
            'create':     [{ argtypes: [ "sampler2D", TObject.VEC2], ret: TObject.VEC4 }],
        },

        methodCall : glMatrixSingletonMethodCall,

        propertySelection : function (name, tEnv, fEnv, ast) {
            var fields = ast.children[0].typeInfo.properties.fields;
            for(var idx in fields) {
                if(name === idx) {
                    return fields[idx];
                }
            }
            reportError("Could not find property", name, "in texture2D");
        },
        makeType : function(val) {
            var type = new TObject(TObject.TEXTURE2D); // ?
            type.updateOpenCLType();
            return type;
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
            console.log('empty update type on texture2D', this);
        },
        setAddressSpace : function (val) {
            //this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        equals : function (other) {
            return other.name === this.name;
        }
    };

    TOp.registry[TObject.ATTRIBUTES] = {
        methodCall : function(thisType, name, tEnv, fEnv, ast) {
            reportError("Methods not supported on Attributes"); // FIXME
        },
        propertySelection : function (name, tEnv, fEnv, ast) {
            // these are strings, not TObjects
            var fields = ast.children[0].typeInfo.properties.typeNames;
            for (var idx in fields) {
                if (name === idx) {
                    type = TObject.makeType(fields[idx]);
                    tEnv.addRoot(type);
                    return type;
                }
            }
            reportError("Could not find property", name, "in Attributes");
        },
        makeType : function(attributeTypes) {
            var type = new TObject(TObject.ATTRIBUTES);
            type.properties = {};
            type.properties.typeNames = attributeTypes; // FIXME clone?
            type.updateOpenCLType();
            return type;
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
        },
        setAddressSpace : function (val) {
            this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        isPseudoType : true,
        equals : function (other) {
            return other === this;
            // others is a singleton type.
        },
        clone: function(other) {
            debugger;
        }

    };
   
    TOp.registry[TObject.UNIFORMS] = {
        methodCall : function(thisType, name, tEnv, fEnv, ast) {
            reportError("Methods not supported on Objects"); // FIXME
        },
        propertySelection : function (name, tEnv, fEnv, ast) {
            // these are strings, not TObjects
            var fields = ast.children[0].typeInfo.properties.typeNames;
            for (var idx in fields) {
                if (name === idx) {
                    var type = TObject.makeType(fields[name]);
                    tEnv.addRoot(type);
                    return type;
                }
            }
            reportError("Could not find property", name, "in Uniforms");
        },
        makeType : function(uniformTypes) {
            var type = new TObject(TObject.UNIFORMS);
            type.properties = {};
            type.properties.typeNames = uniformTypes; // FIXME clone?
            type.updateOpenCLType();
//            tEnv.addRoot(type);
            return type;
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
        },
        setAddressSpace : function (val) {
            this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        isPseudoType : true,
        equals : function (other) {
            // Uniforms is a singleton type.
            return other === this;
        }
    };

    TOp.registry[TObject.VARYINGS] = {
        methodCall : function(thisType, name, tEnv, fEnv, ast) {
            reportError("Methods not supported on Varyings"); // FIXME
        },
        propertySelection : function (name, tEnv, fEnv, ast) {
            var fields = ast.children[0].typeInfo.properties.fields;
            for (var idx in fields) {
                if (name === idx) {
                    return fields[name];
                }
            }
            reportError("Could not find property", name, "in Varyings");
        },
        makeType : function(varyings) {
            var type = new TObject(TObject.VARYINGS);
            // FIXME make copy
            type.properties.fields = varyings;
            type.updateOpenCLType();
            return type;
        },
        getOpenCLSize : function () {
        },
        updateOpenCLType : function () {
        },
        setAddressSpace : function (val) {
            this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },
        constructor : undefined,
        isPseudoType : true,
        equals : function (other) {
            // Varyings is a singleton type.
            return other === this;
        }
    };

    TOp.registry[TObject.ARRAY] = {
        methodCall : function(thisType, name, tEnv, fEnv, ast) {
            switch (name) {
                default:
                    reportError("method `" + name + "` not supported on array objects", ast);
            }
        },
        propertySelection : function (name, tEnv, fEnv, ast) {
            var type;

            switch (name) {
                case "length":
                    type = new TLiteral(TLiteral.NUMBER);
                    break;

                default:
                    reportError("unknown array property `" + name + "`", ast);
            }

            return type;
        },
        constructor : undefined,
        constructors : [Float64Array, Float32Array, Uint32Array, Int32Array, 
                        Uint16Array, Int16Array, Uint8ClampedArray, Uint8Array, Int8Array,
                        RiverTrail.Helper.FlatArray],
        makeType : function (val) {
            var type;
            if (typeof(val) === "number") {
                type = new TLiteral(TLiteral.NUMBER);
            } else if (val instanceof RiverTrail.Helper.FlatArray) {
                type = new TLiteral(TLiteral.NUMBER);
                type.OpenCLType = RiverTrail.Helper.inferTypedArrayType(val.data);
                for (var i = val.shape.length-1; i >= 0; i--) {
                    var ntype = new TObject(TObject.ARRAY);
                    ntype.properties.shape = [val.shape[i]];
                    ntype.properties.elements = type;
                    type = ntype;
                    type.updateOpenCLType();
                }
            } else if (RiverTrail.Helper.isTypedArray(val)) {
                // This is cheating, as typed arrays do not have the same interface, really.
                // However, we do not support map/reduce etc. anyway.
                type = new TObject(TObject.ARRAY);
                type.properties.shape = [val.length];
                type.properties.elements = new TLiteral(TLiteral.NUMBER);
                type.properties.elements.OpenCLType = RiverTrail.Helper.inferTypedArrayType(val);
                type.updateOpenCLType();
            } else {
                reportError("unsupported array contents encountered");
            }
            return type;
        },
        updateOpenCLType : function () {
            var elemType = this.properties.elements;
            var shape = this.getOpenCLShape();
            // FIXME shape should be 1 dimensional
            if (elemType instanceof TLiteral) {
                this.OpenCLType = this.properties.elements.OpenCLType + "[" + shape[0] + "]";
            } else if( (elemType.properties.addressSpace === "__private") && (elemType.isObjectType(TObject.ARRAY) || elemType.isObjectType(TObject.PARALLELARRAY))) {
                // JS : Generate right type for nested local arrays (JS Arrays and ParallelArrays)
                this.OpenCLType = elemType.OpenCLType + "[" + shape[0] + "]";
            } else if (elemType.isObjectType(TObject.ARRAY) || elemType.isObjectType(TObject.PARALLELARRAY)) {
                // TODO: Global arrays of element type T should have type T* here
                //
                this.OpenCLType = elemType.OpenCLType;
            } else if (elemType.isObjectType("InlineObject")) {
                this.OpenCLType = elemType.OpenCLType + "[" + shape[0] + "]";
            } else {
                reportBug("unhandled element type in Array");
            }
        },
        getOpenCLShape : function () {
            return this.properties.shape.concat(this.properties.elements.getOpenCLShape());
        },
        getOpenCLSize : function () {
            return this.properties.shape.reduce(function (prev, curr) { return prev*curr; }, 1) * this.properties.elements.getOpenCLSize();
        },
        equals : function (other) {
            return (this.properties.shape.length === other.properties.shape.length) &&
                   this.properties.shape.every( function (val, idx) { return val === other.properties.shape[idx]; }) &&
                   (this.properties.elements.equals(other.properties.elements));
        },
        setAddressSpace : function (val) {
            this.properties.addressSpace = val;
            this.properties.elements.setAddressSpace(val);
        }
    };

    TOp.registry[TObject.JSARRAY] = {
        methodCall : TOp.registry[TObject.ARRAY].methodCall,
        propertySelection : TOp.registry[TObject.ARRAY].propertySelection,
        constructor : Array,
        makeType : function (val) {
            var type;
            if (typeof(val) === "number") {
                type = new TLiteral(TLiteral.NUMBER);
                type.OpenCLType = "float";
            } else if (val instanceof Array) {
                type = new TObject(TObject.JSARRAY);
                type.properties.shape = [val.length];
                if (val.length > 0) {
                    type.properties.elements = this.makeType(val[0]);
                    if (!lazyJSArrayCheck) {
                        for (var i = 1; i < val.length; i++) {
                            var eType = this.makeType(val[i]);
                            if (!type.properties.elements.equals(eType)) {
                                reportError("inhomogeneous arrays not supported");
                            }
                        }
                    }
                } else {
                    reportError("empty arrays are not supported yet");
                }
                type.updateOpenCLType();
            } else {
                reportError("unsupported array contents encountered");
            }
            return type;
        },
        updateOpenCLType : function () {
            this.OpenCLType = this.properties.elements.OpenCLType + "[" + this.properties.shape[0] + "]";
            /* this type is hardwired */
                ///this.OpenCLType = "/* jsval */ double*";
        },
        getOpenCLShape : TOp.registry[TObject.ARRAY].getOpenCLShape,
        getOpenCLSize : TOp.registry[TObject.ARRAY].getOpenCLSize,
        equals : TOp.registry[TObject.ARRAY].equals,
        setAddressSpace : TOp.registry[TObject.ARRAY].setAddressSpace,
        slangTypedName: function(name)  { 
            return this.properties.elements.slangTypedName(name) + "[" + this.properties.shape[0] + "]";
        }
    };

    TOp.registry[TObject.PARALLELARRAY] = {
        methodCall : function(thisType, name, tEnv, fEnv, ast) {
            "use strict";
            var type;
            ast.children[1] = drive(ast.children[1], tEnv, fEnv);
            var argTypes = tEnv.accu;
            tEnv.resetAccu();

            switch (name) {
                case "get":
                    var idxLen;
                    if ((argTypes.length == 1) &&
                        ((argTypes[0].isObjectType(TObject.ARRAY)) ||
                         (argTypes[0].isObjectType(TObject.PARALLELARRAY)))) {
                        // ensure valid index
                        argTypes[0].isObjectType(TObject.ARRAY) || reportError("invalid index in get call", ast);
                        argTypes[0].properties.shape.length === 1 || reportError("only vectors and scalars are allowed as indices in get", ast);
                        argTypes[0].properties.shape[0] <= thisType.properties.shape.length || reportError("index vector too long", ast);
                        idxLen = argTypes[0].properties.shape[0];
                    } else {
                        // index scalars
                        // a) ensure all are numbers
                        argTypes.every( function (t) { return t.isArithType(); }) || reportError("indices in call to get " +
                                "on parallel array are not numbers", ast);
                        // b) ensure index is not too long
                        argTypes.length <= thisType.properties.shape.length || reportError("too many indices in get call", ast);
                        // get idx length
                        idxLen = argTypes.length;
                    }
                    if (idxLen === thisType.properties.shape.length) {
                        type = thisType.properties.elements.clone();
                        if (type.isNumberType()) {
                            // regardless of the type representation inside of the array, on read we
                            // always convert to the default number type
                            type._castRequired = new TLiteral(TLiteral.NUMBER);
                        }
                    } else {
                        type = new TObject(TObject.PARALLELARRAY);
                        type.properties.shape = thisType.properties.shape.slice(idxLen);
                        type.properties.addressSpace = thisType.properties.addressSpace;
                        type.properties.elements = thisType.properties.elements.clone();
                        type.updateOpenCLType();
                    }
                    // add flow information for dataflow graph
                    type.registerFlow(thisType);
                    // tell the allocator that this result will share the memory of the source
                    if (!type.isScalarType()) {
                        type.properties.isShared = true;
                    }
                    break;

                case "getShape":
                    argTypes.length === 0 || reportError("too many argument to getShape");
                    type = new TObject(TObject.ARRAY);
                    type.properties.shape = [thisType.properties.shape.length];
                    type.properties.elements = new TLiteral(TLiteral.NUMBER);
                    type.properties.addressSpace = "__private"
                    type.updateOpenCLType();
                    tEnv.addRoot(type);
                    break;

                default:
                    reportError("method `" + name + "` not supported for parallel array objects", ast);
            }
            return type;
        },
        propertySelection : function (name, tEnv, fEnv, ast) {
            var type;

            switch (name) {
                case "length":
                    type = new TLiteral(TLiteral.NUMBER);
                    break;

                default:
                    reportError("unknown parallel array property `" + name + "`", ast);
            }

            return type;

        },
        constructor : Float32Array,
        makeType : function (val) {
            var type = new TObject(TObject.PARALLELARRAY);
            // TODO: reflect shape information etc
            type.properties.shape = val.getShape();
            type.properties.elements = new TLiteral(TLiteral.NUMBER); // ParallelArrays always contain numbers
            type.properties.elements.OpenCLType = inferPAType(val).inferredType; // but they may use a different representation
            type.updateOpenCLType();
            return type;
        },
        updateOpenCLType : function () {
            debug && ((this.properties.elements instanceof TLiteral) || reportBug("ParallelArray with non literal elements!"));
            this.OpenCLType = this.properties.elements.OpenCLType + "*";
        },
        getOpenCLShape : function () {
            return this.properties.shape.concat(this.properties.elements.getOpenCLShape());
        },
        getOpenCLSize : function () {
            return this.properties.shape.reduce(function (prev, curr) { return prev*curr; }, 1) * this.properties.elements.getOpenCLSize();
        },
        equals : function (other) {
            return (this.properties.shape.length === other.properties.shape.length) &&
                   this.properties.shape.every( function (val, idx) { return val === other.properties.shape[idx]; }) &&
                   (this.properties.elements.equals(other.properties.elements));
        },
        setAddressSpace : function (val) {
            //this.properties.addressSpace = val;
            //this.properties.elements.setAddressSpace(val);
        },

        slangTypedName: function(name)  { 
            var shape = this.properties.shape;
            if (shape.length == 1) {
                return "vec" + shape[0] + " " + name;
            } else if (shape.length == 2) {
                if (shape[0] !== shape[1]) reportError("non-rectangular mats not supported");
                return "mat" + shape[0] + " " + name;
            }
            return this.properties.elements.slangTypedName(name) + "[" + this.properties.shape[0] + "]";
        }
    };

    //
    // function environment
    //
    var FEnv = function (env) {
        this.parent = env;
        this.bindings = {};
        this.bindings.__proto__ = null;
    };
    var FEp = FEnv.prototype;
    FEp.lookup = function (name) {
        return (this.bindings[name] || (this.parent && this.parent.lookup(name)) || undefined);
    };
    FEp.add = function (f, name, global) {
        if (global && this.parent) {
            this.parent.add(f, name, global);
        } else {
            var fname = name || f.name || reportBug("unnamed functions cannot be added to environment");
            if (this.bindings[fname] !== undefined) reportError("functions need to be uniquely defined within a scope", f);
            this.bindings[fname] = f;
        }
    };
    FEp.toFunDecls = function () {
        var result = [];
        var fun;
        for (var name in this.bindings) {
            fun = this.bindings[name];
            if (fun.specStore) {
                // this is actually called somewhere, so we keep it and all its specialisations
                fun.specStore.forEach(function (f) { result.push(f); });
            }
        }
        return result;
    };

    //
    // main analysis driver
    //
    function drive(ast, tEnv, fEnv) {
        //"use strict"; // seems to trigger a timing-dependent behavior in Chrome

        var left, right;

        if ((ast === null) || (ast === undefined)) {
            reportBug("malformed syntax tree", ast);
        }
        
        switch (ast.type) {
            case CAST:
            case TOINT32:
                // These can only be encountered during a function specialisation. As we recompute
                // them, we can safely scrap those here.
                ast = ast.children[0];
                // fallthrough!
                
            case SCRIPT:
                // create a new type environment for local bindings
                tEnv = new TEnv(tEnv);
                // add all local variable declarations to environment to shadow old
                // ones from previous scopes
                ast.varDecls.forEach(function (name) { 
                    tEnv.bind(name.value, undefined, name.readOnly == true ? name.initializer : null); });
                // add all locally declared functions to the environment
                // strictly speaking they are not variable bindings yet they can shadow variables and be shadowed
                // by variables so we disallow these
                ast.funDecls.forEach(function (f) {
                        f.name || reportBug("unnamed function in funDecls");
                        tEnv.bind(f.name);
                        });
                // add all locally declared functions to the function store. Other than the variable environment, this
                // is about storing their code in case we find a call. 
                fEnv = new FEnv(fEnv);
                ast.funDecls.forEach(function (f) {fEnv.add(f);});
                ast.children.map(function (ast) { return drive(ast, tEnv, fEnv); });
                tEnv.resetAccu();
                // remember symbol table for later phases
                ast.symbols = tEnv;
                // add all locally used functions to funDecls (including the globals we dragged into the scope)
                ast.funDecls = fEnv.toFunDecls();
                // if (ast.funDecls) console.log('typeinference fun decls', ast.funDecls);

                break;
            case BLOCK:
                ast.children.map(function (ast) { return drive(ast, tEnv, fEnv); });
                tEnv.resetAccu();
                break;

            //
            // statements
            //
            case FUNCTION:
                if (ast.functionForm !== Narcissus.parser.DECLARED_FORM) {
                    reportBug("function literals should not be in statement position", ast);
                }
                // this is not an applied occurence but the declaration, so we do not do anything here
                break;
            case RETURN:
                ast.value || reportError("functions need to return a value", ast);
                ast.value = drive(ast.value, tEnv, fEnv);
                tEnv.functionResult = tEnv.accu;
                break;
            case FOR:
                ast.setup = drive(ast.setup, tEnv, fEnv);
                // fallthrough;
            case WHILE:
                ast.condition = drive(ast.condition, tEnv, fEnv);
                var innerEnv = new TEnv(tEnv);
                ast.body = drive(ast.body, innerEnv, fEnv);
                if (ast.update) { // FOR loop
                    ast.update = drive(ast.update, innerEnv, fEnv);
                }
                innerEnv.tagAllUnitialized();
                tEnv.merge(innerEnv);
                break;
            case DO:
                ast.body = drive(ast.body, tEnv, fEnv);
                ast.condition = drive(ast.condition, tEnv, fEnv);
                break;
            case IF:
                ast.condition = drive(ast.condition, tEnv, fEnv);
                tEnv.accu.isTruthType() || reportError("illegal predicate in conditional", ast);
                var thenEnv = new TEnv(tEnv);
                ast.thenPart = drive(ast.thenPart, thenEnv, fEnv);
                var elseEnv = new TEnv(tEnv);
                ast.elsePart && (ast.elsePart = drive(ast.elsePart, elseEnv, fEnv));
                thenEnv.intersect(elseEnv);
                tEnv.merge(thenEnv);
                break;
            case SEMICOLON:
                if (ast.expression) {
                    ast.expression = drive(ast.expression, tEnv, fEnv);
                }
                tEnv.resetAccu();
                break;
            case CONST:
            case VAR:
                ast.children.map(function (ast) {
                                     if (ast.initializer) {
                                         ast.initializer = drive(ast.initializer, tEnv, fEnv);
                                             tEnv.update(ast.name, tEnv.accu, ast.readOnly === true ? ast.initializer : null); 
                                             ast.typeInfo = tEnv.accu;
                                             tEnv.resetAccu();
                                     }
                                     return ast;
                                 });
                break;
            case ASSIGN:
                // children[0] is the left hand side, children[1] is the right hand side.
                // both can be expressions. 
                ast.children[1] = drive(ast.children[1], tEnv, fEnv);
                left = ast.children[0];
                switch (ast.children[0].type) {
                    case IDENTIFIER:
                        // simple case of a = expr
                        tEnv.update(left.value, tEnv.accu);
                        left = drive(left, tEnv, fEnv);
                        break;
                    case INDEX:
                        // array update <expr>[iv] = expr
                        // 1) infer types for lhs
                        left = drive(left, tEnv, fEnv);
                        // 2) figure out what <expr> is. Has to yield an Array object of some sort.
                        left.children[0].typeInfo.isArrayishType() || reportError("illegal object in lhs selection; type seen was " 
                                                                                       + left.children[0].typeInfo, ast);
                        // 3) ensure the update is monomorphic
                        left.typeInfo.equals(ast.children[1].typeInfo) || reportError("mutation of array invalidates types: " 
                                                                                      + left.typeInfo + " updated with " 
                                                                                      + ast.children[1].typeInfo, ast);
                        // 4) the result of the assignment is the rhs...
                        tEnv.accu = ast.children[0].typeInfo.clone();
                        break;
                    case DOT:
                        // Property update on an InlineObject
                        // We have a.b = <expr>
                        // 1) Infer type for lhs's child 0 - should be an inline object
                        // 2) Check if the field (lhs's child 1) is valid
                        // 3) Check if the update is monomorphic
                        left = drive(left, tEnv, fEnv);
                        if(!left.children[0].typeInfo.isObjectType("InlineObject") || 
                                !left.children[0].typeInfo.properties.fields.hasOwnProperty(left.children[1].value))
                            reportError("Invalid field " + left.children[1].value + " referenced on object " + left.children[0].value);
                        tEnv.accu = ast.children[0].typeInfo.clone();
                        break;
                    case ARRAY_INIT:
                        // Destructuring assignment
                        // [a,b,c] = <expr>
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
                ast.children.map(function (ast) { return drive(ast, tEnv, fEnv);});
                // we keep the type of the last child
                break;
            case HOOK:
                // the hook (?) is badly designed. The first child is the condition, second child
                // the then expression, third child the else expression
                ast.children[0] = drive(ast.children[0], tEnv, fEnv);
                tEnv.accu.isTruthType() || reportError("illegal predicate in conditional expression", ast);
                ast.children[1] = drive(ast.children[1], tEnv, fEnv);
                left = tEnv.accu;
                ast.children[2] = drive(ast.children[2], tEnv, fEnv);
                left.equals(tEnv.accu) || reportError( "then and else branch in conditional expression have different types", ast);
                // we create a new node in the DFG
                right = tEnv.accu;
                tEnv.accu = left.clone();
                tEnv.accu.registerFlow(right);
                tEnv.accu.registerFlow(left);
                break;
                
            // binary operations on all literals
            case PLUS: 
                // we do not support strings yet, so this case is the same as numbers
                // fallthrough

            // binary operators on numbers (incl bool)
            case BITWISE_OR:
            case BITWISE_XOR:
            case BITWISE_AND:
            case EQ:
            case NE:
            case STRICT_EQ:
            case STRICT_NE:
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
            case MOD:    
                ast.children[0] = drive(ast.children[0], tEnv, fEnv);
                tEnv.accu.isArithType() || reportError("first argument not a number (found " + tEnv.accu.toString() + ")", ast);
                ast.children[1] = drive(ast.children[1], tEnv, fEnv);
                tEnv.accu.isArithType() || reportError("second argument not a number (found " + tEnv.accu.toString() + ")", ast);
                // result always is a number
                tEnv.accu = new TLiteral(TLiteral.NUMBER);
                break;

            // binary operators on bool
            case OR:
            case AND:
                ast.children[0] = drive(ast.children[0], tEnv, fEnv);
                // XXX what do we allow as legal argument types to OR and AND? For now, numbers should do.
                tEnv.accu.isArithType() || reportError("first argument not a number (found " + tEnv.accu.toString() + ")", ast);
                ast.children[1] = drive(ast.children[1], tEnv, fEnv);
                // XXX same here
                tEnv.accu.isArithType() || reportError("second argument not a number (found " + tEnv.accu.toString() + ")", ast);
                // result always is a bool
                tEnv.accu = new TLiteral( TLiteral.BOOL);
                break;

            // unary functions on all literals
            case NOT:
            case BITWISE_NOT:
            case UNARY_PLUS:
            case UNARY_MINUS:
                // we do not support strings yet, so this is the same as the case below
                // fallthrough

            // unary functions on numbers (incl bool)
            case INCREMENT:
            case DECREMENT:
                ast.children[0] = drive(ast.children[0], tEnv, fEnv);
                tEnv.accu.isArithType() || reportError("argument not a number (found " + tEnv.accu.toString() + ")", ast);
                if (ast.type === NOT) {
                    // result is bool
                    tEnv.accu = new TLiteral(TLiteral.BOOL);
                } else {
                    // result is a number
                    tEnv.accu = new TLiteral(TLiteral.NUMBER);
                }
                break;

            // literals
            case IDENTIFIER:
            case THIS:
            var idType = tEnv.lookup(ast.value) || console.log(tEnv) || reportError("unbound variable: " + ast.value, ast);
                idType.initialized || reportError("variable " + ast.value + " might be uninitialized", ast);
                tEnv.accu = idType.type.clone();
                tEnv.accu.registerFlow(idType.type);
                break;
            case DOT:
                ast.children[0] = drive(ast.children[0], tEnv, fEnv);
                var obj = tEnv.accu;
                obj.isObjectType() || reportError("dot applied to non-object value", ast);
                tEnv.accu = obj.getHandler().propertySelection(ast.children[1].value, tEnv, fEnv, ast);
                break;

            case NUMBER:
                tEnv.accu = new TLiteral(TLiteral.NUMBER);
                break;
            case TRUE:
            case FALSE:
                tEnv.accu = new TLiteral(TLiteral.BOOL);
                break;

            // array operations
            case INDEX:
                ast.children[1] = drive(ast.children[1], tEnv, fEnv);
                tEnv.accu.isArithType() || reportError("index not a number (found " + tEnv.accu.toString() + ")", ast);
                ast.children[0] = drive(ast.children[0], tEnv, fEnv);
                if (tEnv.accu.isObjectType(TObject.ARRAY) || tEnv.accu.isObjectType(TObject.JSARRAY)) {
                    left = tEnv.accu.properties.elements.clone();
                    left.registerFlow(tEnv.accu);
                    tEnv.accu = left;
                } else if (tEnv.accu.isObjectType(TObject.VEC2)) {
                    console.log("should not reach?");
                    var idxVal = Number(ast.children[1].value); // FIXME must be literal
                    if (idxVal == 0 || idxVal == 1) {
                        left = new TLiteral(TLiteral.NUMBER);
                        left.registerFlow(tEnv.accu);
                        tEnv.accu = left;
                    } else {
                        reportError("invalid index to vec2 " + idx, ast);
                    }
                } else if (tEnv.accu.isObjectType(TObject.PARALLELARRAY)) {
                    if (tEnv.accu.properties.shape.length === 1) {
                        // result is a scalar
                        left = tEnv.accu.properties.elements.clone();
                        left.registerFlow(tEnv.accu);
                    } else {
                        // result is a ParallelArray again
                        left = new TObject(TObject.PARALLELARRAY);
                        left.properties.shape = tEnv.accu.properties.shape.slice(1);
                        left.properties.addressSpace = tEnv.accu.properties.addressSpace;
                        left.properties.elements = tEnv.accu.properties.elements.clone();
                        left.updateOpenCLType();
                        left.registerFlow(tEnv.accu);
                    }
                    tEnv.accu = left;
                } else {
                    reportError("Index operator applied to non array value. Type found: " + tEnv.accu.toString(), ast);
                }
                break;

            case ARRAY_INIT:
                left = [];
                for (var idx in ast.children) {
                    if (!ast.children.hasOwnProperty(idx)) continue;
                    ast.children[idx] = drive(ast.children[idx], tEnv, fEnv);
                    left.push(tEnv.accu);
                }
                (left.length > 0) || reportError("empty arrays are not supported", ast);
                left.reduce(function(a,b) { a.equals(b) || reportError("inhomogeneous element types in array initialiser", ast); return a;});
                //tEnv.accu = new TObject(TObject.ARRAY);
                tEnv.accu = new TObject(TObject.JSARRAY); // really, it's a glsl array
                tEnv.accu.properties.elements = left[0].clone();
                tEnv.accu.properties.shape = [ast.children.length];
                tEnv.accu.updateOpenCLType();
                tEnv.addRoot(tEnv.accu);
                // Store flow information for local array. If the elements are scalars, there is no flow information, so this is save.
                // If the elements are arrays themselves, their address space will flow to the local array. As local arrays essentially are
                // arrays of pointers to the elements, they can point to the global address space, although they are local. Also, the n:1
                // flow will automatically demote mixed arrays to the private address space.
                // left.forEach(function (v) { tEnv.accu.registerFlow(v); });
                // this needs a more sophisticated type representation, so we leave it at local for now.
                tEnv.accu.setAddressSpace("__private");
                break;

            // function application
            case CALL:
                switch (ast.children[0].type) {
                    case DOT: // method invocation
                        if(ast.children[0].children[0].value === "RiverTrailUtils") {
                            RiverTrailUtils_Trap(ast, tEnv, fEnv);
                            break;
                        }
                        var dot = ast.children[0];
                        // figure out what type this object is
                        dot.children[0] = drive(dot.children[0], tEnv, fEnv);
                        var objType = tEnv.accu;
                        objType.isObjectType() || reportError("left hand side of method call not an object", ast);
                        // hand off inference to object handler
                        tEnv.accu = objType.getHandler().methodCall(objType, dot.children[1].value, tEnv, fEnv, ast);
                        break;
                    case IDENTIFIER: // function call
                        // grab argument types
                        ast.children[1] = drive(ast.children[1], tEnv, fEnv);
                        var argT = tEnv.accu;
                        tEnv.resetAccu();
                        // grab function
                        var fname = ast.children[0].value;
                        var fun = fEnv.lookup(fname);
                        if (!fun) {
                           if (allowGlobalFuns) {
                               // so this is not a local function. first make sure it is not a local variable
                               !tEnv.lookup(fname) || reportError("not a function `" + fname + "`", ast);
                               // CHEAT: we would have to inspect the current functions closure here but we cannot. So we just
                               //        take whatever the name is bound to in the current scope. 
                               //        This should at least be the global scope, really...
                               var obj = eval(fname) || reportError("unknown function `" + fname + "`", ast);
                               (typeof(obj) === 'function') || reportError("not a function `" + fname + "`", ast);
                               if (fname === "Number") {
                                   // placeholder ast
                                   fun = RiverTrail.Helper.parseFunction("function Number(val) { return val; }");
                                   fun.isBuiltin = true; // note adhoc property
                               } else {
                                   fun = RiverTrail.Helper.parseFunction(obj.toString());
                               }
                               // if we get here, we can just add the function to the function environment for future use
                               fEnv.add(fun, ast.children[0].value, true);
                           } else {
                               reportError("unknown function `" + fname + "`", ast);
                           }
                        }
                        var resType = undefined;
                        var rootFun = fun;
                        if (fun.typeInfo) {
                            // this function has been called before. Try and find the correct specialisation
                            var found;
                            for (var cnt = 0; cnt < fun.specStore.length; cnt++) {
                                if (argT.every(function(t, idx) { return t.equals(fun.specStore[cnt].typeInfo.parameters[idx]);})) {
                                    found = fun.specStore[cnt];
                                    break;
                                }
                            }
                            if (true && found) {
                                resType = found.typeInfo.result;
                                fun = found;
                            } else {
                                // specialize
                                fun = cloneFunctionNoTypes(fun);
                            }
                        } 
                        
                        if (!resType) {
                            // Ensure that the function has a unique, valid name to simplify
                            // the treatment downstream
                            fun.dispatch = nameGen(fun.name);
                            // create a new function frame
                            var innerTEnv = new TEnv(tEnv, true);
                            // put this call on the stack for tracing
                            stackTrace.push({ast: ast, fun: fun});
                            // add parameter / value type mapping
                            fun.params.length === argT.length || reportError("number of parameters and arguments in call does not match", ast);
                            // we clone the argument types here to ensure that later type
                            // upgrades do not propagate to function signatures!
                            fun.params.forEach(function(arg, idx) { innerTEnv.bind(arg); innerTEnv.update(arg, argT[idx].clone()); });
                            // go derive
                            fun.body = drive(fun.body, innerTEnv, fEnv);
                            // initialize specialisation store
                            if (rootFun.specStore === undefined) {
                                rootFun.specStore = [];
                            }
                            rootFun.specStore.push(fun);
                            resType = innerTEnv.functionResult;
                            // drop call from tracing stack
                            stackTrace.pop();
                            // create a new flow frame around this function
                            var innerArgT = fun.params.map(function (v) { return innerTEnv.lookup(v).type; });
                            innerArgT.forEach(function (v) { innerTEnv.addRoot(v); });
                            fun.flowFrame = new FFunction(innerArgT, resType, fun);
                            fun.typeInfo = new TFunction(innerArgT, resType);
                            fun.flowRoots = innerTEnv.getRoots();
                            fun.symbols = innerTEnv;
                            debug && console.log(fun.name + " has type " + fun.typeInfo.toString());
                        }
                        // tie the arguments to the function call
                        ast.callFrame = new FCall(argT, fun.flowFrame, resType.clone(), ast);
                        argT.forEach(function(arg, idx) {arg.registerParamFlow(new FParam(idx, ast.callFrame))});
                        // remember how often this instance is used
                        fun.flowFrame.uses++;
                        // store the name of the instance
                        ast.children[0].dispatch = fun.dispatch;
                        tEnv.accu = ast.callFrame.result;
                        break;

                    default:
                        reportError("unexpected target for function call", ast);
                }
                break;

            // argument lists
            case LIST:      
                left = [];
                for (var idx in ast.children) { 
                    if (!ast.children.hasOwnProperty(idx)) continue;
                    ast.children[idx] = drive(ast.children[idx], tEnv, fEnv);
                    var inner = tEnv.accu.clone();
                    inner.registerFlow(tEnv.accu);
                    left.push(inner);
                }
                tEnv.accu = left;
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
                reportError("break not yet implemented", ast);
				break;
            case CONTINUE:
                reportError("continue not yet implemented", ast);
				break;
            case LABEL:
                reportError("break/continure2/labels not yet implemented", ast);
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
            case NEW_WITH_ARGS:
                if ((ast.children[0].type === IDENTIFIER) &&
                    (ast.children[0].value === "ParallelArray") &&
                    (ast.children[1].type === LIST) &&
                    (ast.children[1].children.length === 1)) { 
                    // special case of new ParallelArray(<expr>)
                    //
                    // this turns into the identity modulo type
                    ast.children[1].children[0] = drive(ast.children[1].children[0], tEnv, fEnv);
                    right = tEnv.accu.clone();
                    if (right.isObjectType(TObject.ARRAY)) {
                        // Change the type. We have to construct the resulting type
                        // by hand here, as usually parallel arrays objects do not
                        // fall from the sky but are passed in or derived from
                        // selections. As this is potentially a nested array,
                        // we have to flatten the type here.
                        right.name = TObject.PARALLELARRAY;
                        right.properties.shape = right.getOpenCLShape();
                        right.properties.elements = function getLast(type) { return type.isScalarType() ? type : getLast(type.properties.elements);}(right);
                        ast.type = FLATTEN;
                        ast.children[0] = ast.children[1].children[0];
                        delete ast.children[1];
                    } else if (right.isObjectType(TObject.PARALLELARRAY)) {
                        // simply get rid of the new
                        ast = ast.children[1].children[0];
                    } else {
                        reportError("Only the simple form of ParallelArray's constructor is implemented", ast);
                    }
                    tEnv.accu = right;
                    break;
                }
                reportError("general object construction not yet implemented", ast);
            case OBJECT_INIT:
                var property_names = [];
                var property_typeInfo = [];
                var fields = {};
                for(var idx in ast.children) {
                    if (!ast.children.hasOwnProperty(idx)) continue;
                    var prop = drive(ast.children[idx], tEnv, fEnv);
                    if(prop.type === PROPERTY_INIT) {
                        property_names.push(prop.typeInfo.name);
                        property_typeInfo.push(prop.typeInfo.tInfo);
                        fields[prop.typeInfo.name] = prop.typeInfo.tInfo;
                    }
                    else {
                        reportError("Unknown element in Object initializer", ast);
                    }
                }
                // Check if we have an equivalent type already. This avoids
                // generating a new type and specializing functions that take
                // this type as a parameter.
                var obj_typeinfo = null; var found = false;
                for(var i = 0; i < globalInlineObjectTypes.length; i++) {
                    var ofields = globalInlineObjectTypes[i].properties.fields;
                    for(var idx in fields) {
                        if(ofields.hasOwnProperty(idx) && ofields[idx].equals(fields[idx])) {
                            obj_typeinfo = globalInlineObjectTypes[i];
                            break;
                        }
                    }
                }
                if(obj_typeinfo === null) {
                    obj_typeinfo = new TObject("InlineObject");
                    obj_typeinfo.properties.fields = fields;
                    // We will defer generating the OpenCLType to actual code
                    // generation
                    obj_typeinfo.baseType = "InlineObj_struct" + labelGen();
                    obj_typeinfo.OpenCLType =  obj_typeinfo.baseType + "*";
                    globalInlineObjectTypes.push(obj_typeinfo);
                }
                tEnv.accu = obj_typeinfo;
                tEnv.accu.setAddressSpace("__private");
                break;
            case PROPERTY_INIT:
                var right = drive(ast.children[1], tEnv, fEnv);
                tEnv.accu = {name:ast.children[0].value, tInfo:ast.children[1].typeInfo};
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

            case DEBUGGER:  // whatever this is...
            default:
                throw "unhandled node type in analysis: " + ast.type + "�is " + RiverTrail.Helper.wrappedPP(ast);
        }
        ast.typeInfo = tEnv.accu;
        debug && ast.typeInfo && console.log(Narcissus.decompiler.pp(ast) + " has type " + ast.typeInfo.toString());
        if (ast.typeInfo && ast.typeInfo._castRequired) {
            var newAst = new Narcissus.parser.Node(ast.tokenizer);
            newAst.children.push(ast);
            newAst.type = CAST;
            newAst.typeInfo = ast.typeInfo._castRequired;
            delete ast.typeInfo._castRequired;
            ast = newAst;
            tEnv.accu = ast.typeInfo;
        }

        return ast;
    }

    // Handle RiverTrailUtils...() calls
    function RiverTrailUtils_Trap(ast, tEnv, fEnv) {
        if(! (ast.children[1].type === LIST) ||
                !(ast.children[1].children.length === 2) ) {
            reportError("Invalid method signature on RiverTrailUtils", ast);
        }
        switch(ast.children[0].children[1].value) {
            case "createArray":
                var elementTypeInfo = drive(ast.children[1].children[1], tEnv, fEnv);
                if(elementTypeInfo.typeInfo.kind === "LITERAL" &&
                        elementTypeInfo.typeInfo.type === "NUMBER") {
                    ast.initializer = ast.children[1].children[1].value;
                }
                else {
                    reportError("Invalid value initializer", ast);
                }
                var objshape = [];
                // Infer shape description
                ast.children[1].children[0] = drive(ast.children[1].children[0], tEnv, fEnv);
                var shapes = ast.children[1].children[0].children;
                var shapes_length = shapes.length;
                for(var idx = 0; idx < shapes_length; idx++) {
                    if(shapes[idx].typeInfo.kind !== "LITERAL" ||
                            shapes[idx].typeInfo.type !== "NUMBER" ||
                            shapes[idx].type !== 61) {
                        reportError("Shape description must consist of literals only, e.g: [3, 4, 2]", ast);
                    }
                    objshape.push(shapes[idx].value);
                }
                tEnv.accu = new TObject(TObject.ARRAY);
                var elements = [];
                var d;
                var top_level_type = "";
                for(d = 0; d < objshape.length; d++) {
                    top_level_type += "*";
                }
                for(d = 0; d < objshape.length; d++) {
                    if(d === objshape.length-1) {
                        elements[d] = elementTypeInfo.typeInfo;
                        elements[d].properties = {};
                    }
                    else {
                        elements[d] = new TObject(TObject.ARRAY);
                        elements[d].OpenCLType = elementTypeInfo.typeInfo.OpenCLType +
                            top_level_type.slice(0, top_level_type.length - d - 1);
                        elements[d].properties = {};
                        elements[d].properties.shape = [objshape[d+1]];
                        elements[d].properties.addressSpace = "__private";
                    }
                    if(d > 0) elements[d-1].properties.elements = elements[d];
                }
                tEnv.accu.properties.elements = elements[0];
                // Given an n x m x p array, the shape in 'typeInfo' for this ast node
                // is 'n'.
                tEnv.accu.properties.shape = [objshape[0]];
                tEnv.accu.updateOpenCLType();
                tEnv.addRoot(tEnv.accu);
                tEnv.accu.setAddressSpace("__private");
                break;
            default:
                reportError("Invalid method called on RiverTrailUtils", ast);
        }
    }

    function typeOracle(val) {
            "use strict";
        var type;

        switch (typeof(val)) {
            case "number":
                type = new TLiteral(TLiteral.NUMBER);
                break;
            case "object":
                var name = TObject.deriveObjectType(val) || reportError("unsupported object as argument encountered");
                var type = TObject.makeType(name, val);
                type.setAddressSpace("__global");
                break;
            default:
                reportError("unsupported argument kind encountered");
        };
        
        return type;
    }


    // 
    // data flow graph for address space forwarding
    //
    // The graph is double linked, directed. It consits of three kinds of nodes:
    //
    // Type objects: these are data objects in the DFG.
    // FFunction objects: these keep the ins and outs of the DFG of a function in one place.
    // FCall objects: encode a call site
    // FParam objects: these are pointers into a FFunction object, encoding arguments that flow in.
    //
    var FlowNode = function () {
        return this;
    };
    var FFunction = function (params, result, root, ast) {
        this.params = params;
        this.result = result;
        this.root = root;
        this.ast = ast || root;
        this.uses = 0;
        this.label = labelGen();
    };
    FFunction.prototype = new FlowNode();
    var FCall = function (params, frame, result, ast) {
        this.params = params;
        this.frame = frame;
        this.result = result;
        this.ast = ast;
        this.label = labelGen();
    };
    FCall.prototype = new FlowNode();
    var FParam = function (number, call) {
        this.number = number;
        this.call = call;
        this.label = labelGen();
    };
    var FPp = FParam.prototype = new FlowNode();
    FPp.getTarget = function () {
        return this.call.frame.params[this.number];
    };
    FPp.getFrame = function () {
        return this.call.frame;
    };
    FPp.redispatch = function (frame) {
        this.call.frame.uses--;
        this.call.frame = frame;
        this.call.frame.uses++;
        this.call.ast.children[0].dispatch = this.call.frame.ast.dispatch;
    };
    FPp.getCall = function () {
        return this.call;
    };

    function resetAddressSpaces(roots) {
        var seen = [];
        var workset = roots.map(function (val) {
                !val._reset || reportBug ("leftover reset flow information!");
                val._reset = true;
                debug && console.log("RESET: adding root " + val.toString());
                return val;
            });
        while (workset.length > 0) {
            var current = workset.pop();
            seen.push(current);
            if (current.flowTo) {
                current.flowTo.forEach( function (v) {
                    if (!v._reset) {
                        v._reset = true;
                        v.setAddressSpace && v.setAddressSpace(undefined);
                        workset.push(v);
                    }
                });
            } else if (current instanceof FParam) {
                if (!current.call._reset) {
                    current.call._reset = true;
                    workset.push(current.call);
                }
            } else if (current instanceof FCall) {
                if (!current.result._reset) {
                    current.result._reset = true;
                    workset.push(current.result);
                }
            }
        }
        seen.forEach( function (v) { delete v._reset; });
    };

    function propagateAddressSpaces(roots) {
        var workset = roots.map(function (val) { 
                !val._flow || reportBug ("leftover flow information!");
                val._flow = true;
                debug && console.log("FLOW: adding root " + val.toString());
                return val;
            });
        var mergeFlow = function (val, currentAS) {
            if (!val._flowVisited && !val._flow) {
                // every node is visited once no matter what
                workset.push(val);
                val._flow = true;
            }
            if (!val.hasAddressSpace()) {
                val.setAddressSpace(currentAS);
                if (!val._flow) {
                    workset.push(val);
                    val._flow = true;
                }
                debug && console.log("propagated address space " + currentAS);
            } else if (val.getAddressSpace() !== currentAS) {
                if (val.getAddressSpace() !== "__private") {
                    val.setAddressSpace("__private");
                    if (!val._flow) {
                        workset.push(val);
                        val._flow = true;
                    }
                    debug && console.log("privatized address space due to conflict");
                }
            } else {
                debug && console.log("address space remains " + val.properties.addressSpace);
            }
        };

        while (workset.length > 0) {
            var current = workset.pop();
            delete current._flow;
            current._flowVisited = true;
            var currentAS = current.getAddressSpace();
            debug && console.log("FLOW: processing " + current.toString() + " with addressspace " + currentAS);
            if (current.flowTo !== undefined) {
                current.flowTo.forEach(function (val) {
                    if (val instanceof FParam) {
                        // we have a function call, which might need specialisation
                        var target = val.getTarget();
                        var frame = val.getFrame();
                        debug && console.log("inspecting call " + RiverTrail.Helper.wrappedPP(val.getCall().ast) + " currently at " + frame.ast.dispatch);
                        debug && console.log("signature is " + frame.params.reduce(function (p,v) { return p + " " + v.getAddressSpace(); }, "") + ", propagating " + val.number + " as " + currentAS);
                        // we do not propagate undefined address spaces (like scalar arguments)
                        if ((currentAS !== undefined) && (!target.hasAddressSpace() || (target.getAddressSpace() !== currentAS))) {
                            // first, we try to find a matching specialization
                            var specs = frame.root.adrSpecStore;
                            var match = undefined;
                            if (specs) {
                                specs.some(function (v) { 
                                        if (v.typeInfo.parameters.every(function (v,idx) {
                                                if (idx === val.number) {
                                                    // current arg
                                                    return (v.getAddressSpace() === currentAS);
                                                } else {
                                                    return (v.getAddressSpace() === frame.params[idx].getAddressSpace());
                                                }
                                            })) {
                                            match = v;
                                            return true;
                                        } else {
                                            return false;
                                        }
                                    });
                            }
                            if (match) {
                                // redispatch things
                                debug && console.log("redispatching call " + RiverTrail.Helper.wrappedPP(val.getCall().ast) + " to " + match.dispatch);
                                val.redispatch(match.flowFrame);
                                target = val.getTarget();
                                frame = val.getFrame();
                            } else {
                                // we need to create a new version
                                if (frame.uses !== 1) {
                                    // we share this call site, so create a new specialisation
                                    var newfun = cloneFunction(frame.ast);
                                    debug && console.log("specializing call " + RiverTrail.Helper.wrappedPP(val.getCall().ast) + " to " + newfun.dispatch);
                                    // store this specialisation
                                    if (!frame.root.adrSpecStore) {
                                        // setup store for specialisations
                                        frame.root.adrSpecStore = [frame.root];
                                    }
                                    frame.root.adrSpecStore.push(newfun);
                                    val.redispatch(newfun.flowFrame);
                                    // update local state
                                    target = val.getTarget();
                                    frame = val.getFrame();
                                } else {
                                    debug && console.log("re-specializing call " + RiverTrail.Helper.wrappedPP(val.getCall().ast) + " instance " + frame.ast.dispatch);
                                }
                                // propagate new information into function
                                target.setAddressSpace(currentAS);
                                frame._flowVisited = true;
                                debug && console.log("looking into function " + frame.ast.dispatch);
                                debug && console.log("signature is " + frame.params.reduce(function (p,v) { return p + " " + v.getAddressSpace(); }, ""));
                                resetAddressSpaces(frame.ast.flowRoots);
                                propagateAddressSpaces(frame.ast.flowRoots);
                                debug && console.log("done with function " + frame.ast.dispatch);
                            }
                        } else if (!frame._flowVisited) {
                            // we have to look at each function at least once
                            debug && console.log("looking into function " + frame.ast.dispatch);
                            frame._flowVisited = true;
                            propagateAddressSpaces(frame.ast.flowRoots);
                            debug && console.log("done with function " + frame.ast.dispatch);
                        }
                        // apply the new result address space to the return node
                        if (frame.result.hasAddressSpace()) {
                            mergeFlow(val.getCall().result, frame.result.getAddressSpace());
                        }
                        debug && console.log("signature of " + RiverTrail.Helper.wrappedPP(val.getCall().ast) + " now is " + frame.params.reduce(function (p,v) { return p + " " + v.getAddressSpace(); }, ""));
                    } else {
                        // merge flow information
                        mergeFlow(val, currentAS);
                    }
                });
            }
        }
    }

    function insertSpecialisations(ast, where) {
        (ast.type === FUNCTION) || reportBug("unexpected node found");
        if (ast.adrSpecStore) {
            where || reportBug("fun specs found but no target to insert into");
            ast.adrSpecStore.forEach(function (v,idx) { if ((idx>0) && (v.flowFrame.uses > 0)) { where.push(v); } });
        }
        ast.body.funDecls.forEach(function (v) {insertSpecialisations(v, ast.body.funDecls);});
    }

    function analyze(ast, pa, construct, attributeTypes, uniformTypes, varyingTypes, lowPrecision) {
        var tEnv = new TEnv(rootEnvironment, true); // create a new top-level function frame
        var params = ast.params;
        var argT = [];
        // clear away old stack traces
        (stackTrace.length === 0) || (stackTrace = []);

        // create type info for all arguments
        if (attributeTypes !== undefined) { 
            var attributes = params[0];
            tEnv.bind(attributes);
            var type = TObject.makeType(TObject.ATTRIBUTES, attributeTypes || {});
            tEnv.update(attributes, type);
            argT.push(type);

            var uniforms = params[1];
            if (uniforms !== undefined) {
                tEnv.bind(uniforms);
                type = TObject.makeType(TObject.UNIFORMS, uniformTypes || {});
                tEnv.update(uniforms, type);
                argT.push(type);
            }
        } else {
            // no attributes, so it's a fragment shader
            var uniforms = params[0];
            tEnv.bind(uniforms);
            var type = TObject.makeType(TObject.UNIFORMS, uniformTypes);
            tEnv.update(uniforms, type);
            argT.push(type);
            
            var varyings = params[1];
            if (varyings !== undefined) {
                tEnv.bind(varyings);
                tEnv.update(varyings, TObject.makeType(TObject.VARYINGS, varyingTypes || {}));
                argT.push(type);
            }
        }

        /*
        params.forEach(function (name) { tEnv.bind(name); });
        params.forEach(function (name, idx) { var type = typeOracle(extraArgs[idx]); 
                                              tEnv.update(name, type); 
                                              type.isObjectType() && tEnv.addRoot(type);
                                              argT.push(type);});
                                              */

        ast.body = drive(ast.body, tEnv, undefined);

        var type = new TFunction(argT, tEnv.functionResult);
        ast.typeInfo = type;
        ast.symbols = tEnv;

        //�propagate address space qualifiers
        propagateAddressSpaces(tEnv.getRoots());
        insertSpecialisations(ast);
                    
        debug && console.log("Overall function has type (first arg. is this) " + type.toString());
        debug && console.log(RiverTrail.dotviz.plotTypes(tEnv._roots));

        return ast;
    }

    return {
        "analyze" : analyze,
        "Type" : Type,
        "TLiteral" : TLiteral,
        "TObject" : TObject,
        "TFunction" : TFunction,
        "FlowNode" : FlowNode,
        "FFunction" : FFunction,
        "FParam" : FParam,
        "FCall" : FCall,
        "typeOracle" : typeOracle
    };
}();
;
/*
  Copyright (c) 2013 Adobe Systems Incorporated. All rights reserved.
  
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at
  
  http://www.apache.org/licenses/LICENSE-2.0
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  

 * Copyright (c) 2011, Intel Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, 
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, 
 *   this list of conditions and the following disclaimer in the documentation 
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

if (ShaderDSL === undefined) {
    var ShaderDSL = {};
}

ShaderDSL.InferMem = function () {
    var definitions = Narcissus.definitions;
    eval(definitions.consts);
    eval(RiverTrail.definitions.consts);

    var isGlMatrixSingleton = ShaderDSL.isGlMatrixSingleton;

    const debug = false;

    // A MemSet models a set of memory variables for a given size. Optinally,
    // each memory name might be associated with a set of aliases that may
    // refer to the same memory. Such aliases are created when two memory sets
    // are overlayed, e.g., because they correspond to two independent control
    // flow paths.
    var MemSet = function () {
        var unique = {value : 0};
        return function () {
            this._store = {};
            this._store.__proto__ = null;
            this._unique = unique;
            return this;
        };
    }();
    var MSP = MemSet.prototype = {};
    MSP.create = function create (name) {
        var memName = "_tmp_" + name + "_" + this._unique.value++;
        this.add(memName);
        return memName;
    }
    MSP.add = function add (name) {
        this._store[name] = null;
    }
    MSP.union = function union (other) {
        for (var mem in other._store) {
            if (other._store[mem] !== null) {
                // this entry has a set of aliases attached to it, so copy
                this._store[mem] = new MemSet();
                this._store[mem].union(other._store[mem]);
            } else {
                this.add(mem);
            }
        }
    };
    MSP.overlay = function overlap (other) {
        var keys = Object.keys(this._store);
        var keyPos = 0;
        for (var mem in other._store) {
            var ms;
            if (keyPos < keys.length) {
                if (this._store[keys[keyPos]] === null) {
                    this._store[keys[keyPos]] = new MemSet();
                }
                ms = this._store[keys[keyPos]];
                ms.add(mem);
            } else {
                this.add(mem);
                ms = this._store[mem] = new MemSet();
            }
            if (other._store[mem] !== null) {
                for (var alias in other._store[mem]._store) {
                    ms.add(alias);
                }
            }
            keyPos++;
        }
    };
    MSP.declare = function declare (kind) {
        var result = "";
        for (var name in this._store) {
            result += kind + " " + name + ";"; // FIXME this is not proper decl for arrays
            if (this._store[name] !== null) {
                result += this._store[name].declareAlias(name);
            }
        };
        return result;
    };
    MSP.declareAlias = function declareAlias (alias) {
        result = "";
        for (var name in this._store) {
            result += "char *" + name + " = " + alias + ";";
        }
        return result;
    }

    // A memory list models a mapping from memory sizes to memory variables.
    // I sort them by sizes so that I can more easily overlay different
    // memory lists.
    var MemList = function () {
        this._store = [];
        return this;
    };
    var MLP = MemList.prototype = {};
    MLP.allocate = function allocate (type, name) {
        !(type instanceof ShaderDSL.Typeinference.TObject) && reportError("Wrong type " + type);
        var kind = type.OpenCLType;
        if (this._store[kind] === undefined) {
            this._store[kind] = new MemSet();
        }
        return this._store[kind].create(name);
    };
    MLP.join = function join (other) {
        for (var kind in other._store) {
            if (!other._store.hasOwnProperty(kind)) continue;
            if (this._store[kind] === undefined) {
                this._store[kind] = new MemSet();
            }
            this._store[kind].union(other._store[kind]);
        }
    };
    MLP.overlay = function overlay (other) {
        for (var kind in other._store) {
            if (!other._store.hasOwnProperty(kind)) continue;
            if (this._store[kind] === undefined) {
                this._store[kind] = new MemSet();
            }
            this._store[kind].overlay(other._store[kind]);
        }
    };
    MLP.declare = function declare () {
        var result = "";
        for (var kind in this._store) {
            if (!this._store.hasOwnProperty(kind)) continue;
            result += this._store[kind].declare(kind);
        }
        return result;
    };

    //
    // error reporting
    //
    function reportError(msg, t) {
        throw "Error: " + msg + " <" + (t ? RiverTrail.Helper.wrappedPP(t) : "no context") + ">"; // could be more elaborate
    }
    function reportBug(msg, t) {
        throw "Bug: " + msg; // could be more elaborate
    }

    var isArrayLiteral = RiverTrail.Helper.isArrayLiteral;

    // The code below creates a single buffer for each
    // dimension of the nested array. These buffers are
    // attached to the AST node and the backend emits code
    // for initializing the pointers in each of these
    // buffers.
    function allocateArrayMem(ast, prefix, memVars) {
        var shape = ast.typeInfo.getOpenCLShape();
        var shape_len = shape.length;
        if(shape_len === 1) {
            ast.allocatedMem = memVars.allocate(ast.typeInfo, prefix);
        }
        else {
            ast.memBuffers = {size:0, list:[]};
            /*
            var redu = 1;
            for(var i = 0; i < shape_len; i++) {
                //var type_size = getTypeSize(i, shape, ast.typeInfo.OpenCLType);
                var type_size = RiverTrail.Helper.getOpenCLSize(ast.typeInfo.OpenCLType);
                var allocation_size = type_size*shape[i]*redu;
                //debug && console.log("Allocating " + allocation_size + " bytes in " +  "CALL_"
                //        + i + "  for i = " + i);
                var memBufferName = memVars.allocate(allocation_size, "CALL_" + i);
                ast.memBuffers.size +=1;
                ast.memBuffers.list.push(memBufferName);

                redu = redu*shape[i];
            }
            */
            var memBufferName = memVars.allocate(ast.typeInfo, "CALL_" + ast.typeInfo.OpenCLType + i);
            ast.memBuffers.size +=1;
            ast.memBuffers.list.push(memBufferName);

            // Set the primary memory buffer for this node to be the
            // top-level buffer
            ast.allocatedMem = ast.memBuffers.list[0];
            //debug && console.log("Total AST allocations: ", ast.memBuffers.size, ast.memBuffers.list.length);
        }
    }


    // We allocate memory for the fields of the object
    // and for the object itself (pointer to the fields)
    function allocateObjMem(ast, memVars) {
        var fields = ast.typeInfo.properties.fields;
        var objSize = 0;
        ast.memBuffers = {__size:0, __root:null};
        for(var idx in fields) {
            ast.memBuffers[idx] = [];
            if(fields[idx].isScalarType()) {
                var allocation_size = RiverTrail.Helper.getOpenCLSize(fields[idx].OpenCLType);
                objSize += allocation_size;
            }
            else if (fields[idx].name === "InlineObject") {
                reportError("InlineObject type properties not implemented yet");
            }
            else if(fields[idx].name === "Array") {
                var shape = fields[idx].getOpenCLShape();
                var shape_len = shape.length;

                objSize += RiverTrail.Helper.getOpenCLSize(fields[idx].OpenCLType);
                var redu = 1;
                for(var i = 0; i < shape_len; i++) {
                    //var type_size = getTypeSize(i, shape, ast.typeInfo.OpenCLType);
                    var type_size = RiverTrail.Helper.getOpenCLSize(fields[idx].OpenCLType);
                    var allocation_size = type_size*shape[i]*redu;
                    debug && console.log("Allocating " + allocation_size + " bytes in " +  "FLD_" + i + "  for i = " + i);
                    var memBufferName = memVars.allocate(fields[idx], "FLD_" + i); // FIXME?
                    ast.memBuffers.__size +=1;
                    ast.memBuffers[idx].push(memBufferName);
                    redu = redu*shape[i];
                }
            }
            else {
                reportError("Unknown field type");
            }
        }
        // Allocate space for the fields of the object
        var obj_memBufferName = memVars.allocate(objSize, "OBJ");
        ast.memBuffers.size += 1;
        ast.memBuffers.__root = obj_memBufferName;
        ast.allocatedMem = obj_memBufferName;
    }

    function lvaluep(acc) {
        return acc && (acc.type == IDENTIFIER || acc.type == INDEX);
    }
    
    function infer(ast, memVars, ins, outs) {
        //"use strict"; // seems to trigger a timing-dependent behavior in Chrome

        switch (ast.type) {
            case SCRIPT:
                ast.funDecls.forEach(function (f) {infer(f.body);});
                ast.memVars = new MemList();
                ast.children.forEach(function (child) { infer(child, ast.memVars, null, null); });
                break;

            case BLOCK:
                ast.children.forEach(function (child) { infer(child, memVars, ins, outs); });
                break;

            //
            // statements
            //
            case FUNCTION:
                // this is not an applied occurence but the declaration, so we do not do anything here
                break;
            case RETURN:
                // special case: if the value is an ARRAY_INIT that only contains allocation free
                // expressions, we do not allocate space for the frame as it is directly written
                if (!isArrayLiteral(ast.value)) {
                    infer(ast.value, memVars, ins, outs);
                }
                break;
            //
            // loops
            //
            case DO:
                // fallthrough;
            case FOR:
                // setup is run once
                if (ast.setup) {
                    infer(ast.setup, memVars, ins, outs);
                }
                // fallthrough;
            case WHILE:
                infer(ast.condition, memVars, ins, outs);
                infer(ast.body, memVars, ast.ins, ast.outs);
                if (ast.update) {
                    infer(ast.update, memVars, ast.ins, ast.outs);
                }
                break;
            case IF:
                infer(ast.condition, memVars, ins, outs);
                var thenMem = new MemList();
                infer(ast.thenPart, thenMem, ins, outs);
                if (ast.elsePart) {
                    var elseMem = new MemList();
                    infer(ast.elsePart, elseMem, ins, outs);
                    thenMem.overlay(elseMem);
                }
                memVars.join(thenMem);
                break;
            case SEMICOLON:
                if (ast.expression) {
                    infer(ast.expression, memVars, ins, outs);
                }
                break;
            case CONST:
            case VAR:
                ast.children.forEach(function (ast) {
                                         if (ast.initializer) {
                                             infer(ast.initializer, memVars, ins, outs);
                                         }
                                     });
                break;
            case ASSIGN:
                // children[0] is the left hand side, children[1] is the right hand side.
                // both can be expressions. 
                infer(ast.children[0], memVars, ins, outs);
                infer(ast.children[1], memVars, ins, outs);
                switch (ast.children[0].type) {
                    case IDENTIFIER:
                        // a = expr
                        //
                        // case 1:
                        // If <expr> is in the __private address space, then if <a> is an in and out var we have to copy, 
                        // as the memory we have allocated for <expr> could potentially be reused in the next iteration 
                        // of the loop before <a> has been read.
                        //
                        // case 2:
                        // If <expr> is in a different address space than <a>, we have to copy, too.
                        var aVar = ast.children[0];
                        if (((ast.children[1].typeInfo.getOpenCLAddressSpace() === "__private") && // case 1
                            (ins && ins.contains(aVar.value) && outs && outs.contains(aVar.value))) ||
                            (aVar.typeInfo.getOpenCLAddressSpace() != ast.children[1].typeInfo.getOpenCLAddressSpace())) { // case 2
                            if(!ast.typeInfo.isScalarType()) {
                                var shape = ast.typeInfo.getOpenCLShape();
                                var shape_len = shape.length;
                                debug && console.log("Creating memory for " + ast.children[0].value + " with shape: ", shape);
                                ast.memBuffers = {size:0, list:[]};
                                var redu = 1;
                                for(var i = 0; i < shape_len; i++) {
                                    //var type_size = getTypeSize(i, shape, ast.typeInfo.OpenCLType);
                                    var type_size = RiverTrail.Helper.getOpenCLSize(ast.typeInfo.OpenCLType);
                                    var allocation_size = type_size*shape[i]*redu;
                                    debug && console.log("Allocating " + allocation_size + " bytes in " +  ast.children[0].value
                                      + "_" + i + "  for i = " + i);
                                    var memBufferName = memVars.allocate(allocation_size, ast.children[0].value + "_" + i); 
                                    ast.memBuffers.size +=1;
                                    ast.memBuffers.list.push(memBufferName);

                                    redu = redu*shape[i];
                                }
                                // Set the primary memory buffer for this node to be the
                                // top-level buffer
                                ast.allocatedMem = ast.memBuffers.list[0];
                                debug && console.log("Total AST allocations: ", ast.memBuffers.size, ast.memBuffers.list.length); 
                            }
                        }
                        break;
                    case INDEX:
                        // case of a[iv] = expr. 
                        break;
                    case DOT:
                        // Support for updates on object properties.
                        infer(ast.children[0], memVars, ins, outs);
                        infer(ast.children[1], memVars, ins, outs);
                        break;
                    default:
                        reportBug("unhandled lhs in assignment");
                        break;
                }
                break;
                
            // 
            // expressions
            //

            case HOOK:
                // the hook (?) is badly designed. The first child is the condition, second child
                // the then expression, third child the else expression
                infer(ast.children[0], memVars, ins, outs);
                var thenMem = new MemList();
                infer(ast.children[1], thenMem, ins, outs); 
                var elseMem = new MemList();
                infer(ast.children[2], elseMem, ins, outs);
                thenMem.overlay(elseMem);
                memVars.join(thenMem);
                break;
                
            // literals
            case IDENTIFIER:
            case THIS:
            case NUMBER:
            case TRUE:
            case FALSE:
                // nothing to do here
                break;

            case ARRAY_INIT: // FIMXE need allocation?
            // fallthru
            // stuff where we just look at the children
            case COMMA:
            case INCREMENT:
            case PLUS: 
            case DECREMENT:
            case MINUS:
            case MUL:
            case EQ:
            case NE:
            case STRICT_EQ:
            case STRICT_NE:
            case LT:
            case LE:
            case GE:
            case GT:
            case BITWISE_OR:
            case BITWISE_XOR:
            case BITWISE_AND:
            case LSH:
            case RSH:
            case URSH:
            case DIV:
            case MOD:    
            case AND: 
            case OR:
            case NOT:
            case UNARY_PLUS:
            case UNARY_MINUS:
            case BITWISE_NOT:
            case DOT:
            case INDEX:
            case LIST:      
            case CAST:
            case FLATTEN:
            case TOINT32:
                if (ast.children) {
                    ast.children.forEach( function (child) { infer(child, memVars, ins, outs); });
                }
                break;
            case CALL: 
                if (ast.children) {
                    if(ast.children[0].type === DOT && ast.children[0].children[0].value === "RiverTrailUtils") {
                        switch (ast.children[0].children[1].value) {
                            case "createArray":
                            allocateArrayMem(ast, "createArray",  memVars);
                                break;
                            default:
                                reportError("Invalid method " + ast.children[0].children[1].value + " on RiverTrailUtils", ast);
                        }
                    }
                    else {
                        ast.children.forEach( function (child) { infer(child, memVars, ins, outs); });
                    }
                }

                if(ast.typeInfo.name === "InlineObject") {
                    allocateObjMem(ast, memVars);
                }
                // If I am returning an Array space needs to be allocated for it in the caller and 
                // the name of the space should be left in the CALL nodes allocatedMem field so that when
                // I generate the call it is available. However, if this method does return a pointer
                // to some existing data, like |get| on ParallelArray, the type inference will have
                // left an isShared annotation and no memory needs to be allocated.
                else if (!ast.typeInfo.isScalarType()) { 
                    // This call returns a nested array. The caller needs to allocate enough
                    // memory for this array and initialize the pointers in
                    // the allocated buffer to create a structure that the
                    // callee can simply fill the leaves of.

                    var receiver = ast.children[0].children[0];
                    if (receiver === undefined || !isGlMatrixSingleton(receiver.value)) {
                        // don't bother
                        break;
                    }
                    var callName = ast.children[0].children[1].value;
                    // Recognizes calls to methods with accumulator of the form:
                    // mat4.multiply(a, b, expr); // expr is not a lvalue
                    // these can't be translated to expr = a * b;
                    // so a temporary is allocated: float tmp = expr; tmp = a * b;
                    // which could be collapsed to tmp = a * b, if it weren't for side effects in expr.

                    switch (callName) {
                    case "negate": 
                    case "multiply":
                    case "subtract":
                    case "divide":
                    case "add": {
                        var args = ast.children[1].children;
                        var acc = args[args.length - 1];
                        if (!lvaluep(acc)) {
                            console.log("allocateArrayMem", Narcissus.decompiler.pp(ast), memVars.declare());
                            allocateArrayMem(ast, callName, memVars);
                        }
                    }
                        break;
                    default:
                        
                    }
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
            case CONTINUE:
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
                for(var idx = 0; idx < ast.children.length; idx++) {
                    infer(ast.children[idx].children[1], memVars, ins, outs);
                }
                break;
            case NEW_WITH_ARGS:
            case OBJECT_INIT:
                for(var idx = 0; idx < ast.children.length; idx++) {
                    infer(ast.children[idx].children[1], memVars, ins, outs);
                }
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

            case DEBUGGER:  // whatever this is...
            default:
                throw "unhandled node type in analysis: " + ast.type;
        }

    };

    function doInfer (ast) {
        if (ast.type !== FUNCTION) {
            reportBug("you probaly wanted to call the inference on a function node!");
        } else {
            infer(ast.body);
        }
    };

    return {
        "infer" : doInfer
    };
}();
