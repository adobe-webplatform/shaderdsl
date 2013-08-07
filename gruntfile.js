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

module.exports = function(grunt) {
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        concat: {
            options: {
                stripBanners: true,
                separator: ';\n',
                banner: '/*! GENERATED FILE, DO NOT EDIT */\n',
            },
            dist: {
                src: ['third-party/rivertrail/jslib/jit/narcissus/jsdefs.js', 
                      'third-party/rivertrail/jslib/jit/narcissus/jslex.js', 
                      'third-party/rivertrail/jslib/jit/narcissus/jsparse.js', 
                      'third-party/rivertrail/jslib/jit/narcissus/jsdecomp.js', 
                      'third-party/rivertrail/jslib/jit/compiler/definitions.js', 
                      'third-party/rivertrail/jslib/jit/compiler/helper.js', 
                      'src/genglsl.js',
                      'src/typeinference.js',
                      'src/infermem.js'
                     ],
                dest: '<%= pkg.name %>'
            }
        },
    });
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.registerTask('default', ['concat']);

}
