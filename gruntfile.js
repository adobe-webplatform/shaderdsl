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
