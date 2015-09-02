var fs = require('fs');
var path = require('path');
var assert = require('assert');

var parser = require('..');

var tests = fs.readdirSync(__dirname);
var errorMessages = fs.readdirSync(path.join(__dirname, 'errors'));

var isBenchmarkMode = process.argv.indexOf('--benchmark') !== -1;

describe('parse: (String) => Object', function() {

    if (isBenchmarkMode) {
        this.timeout(0);
    }

    tests
        .filter(function(name) {
            if (isBenchmarkMode) {
                return name === 'benchmark';
            }

            return (
                name !== 'errors' &&
                name !== 'ignore_html_tags' &&
                name !== 'benchmark' &&
                name.indexOf('.') === -1
            );
        })
        .forEach(function(name) {
            it(name, function() {

                var tmpl = fs.readFileSync(
                    path.join(__dirname, name, 'template.tmpl'),
                    'utf8');

                var expected = JSON.parse(
                    fs.readFileSync(
                        path.join(__dirname, name, 'ast.json'),
                        'utf8'));

                var actual = parser.parse(tmpl, { reducePositionLookups: isBenchmarkMode });

                if (isBenchmarkMode) {
                    assert.deepEqual([], expected);
                } else {
                    assert.deepEqual(actual, expected);
                }

            });
        });
});

describe('error messages', function() {

    errorMessages
        .filter(function(filename) {
            if (isBenchmarkMode) {
                return false;
            }

            return filename.indexOf('template.') === 0;
        })
        .forEach(function(filename) {
            var name = path.basename(filename, '.tmpl');
            var messageFilename = name.replace(/^template/, 'error') + '.json';

            it(name, function() {

                var tmpl = fs.readFileSync(
                    path.join(__dirname, 'errors', filename),
                    'utf8');

                var message = JSON.parse(
                    fs.readFileSync(
                        path.join(__dirname, 'errors', messageFilename),
                        'utf8'));

                try {
                    parser.parse(tmpl);
                    throw new Error('No error thrown');
                } catch(e) {
                    assert.deepEqual(
                        errorAsObject(e),
                        message);
                }

            });
        });

});

describe('options', function() {
    it('ignoreHTMLTags', function() {

        var tmpl = fs.readFileSync(
            path.join(__dirname, 'ignore_html_tags', 'template.tmpl'),
            'utf8');

        var expected = JSON.parse(
            fs.readFileSync(
                path.join(__dirname, 'ignore_html_tags', 'ast.json'),
                'utf8'));

        var actual = parser.parse(tmpl, { ignoreHTMLTags: true });

        assert.deepEqual(actual, expected);

    });
});

function errorAsObject(e) {
    return Object.getOwnPropertyNames(e).reduce(function(acc, key) {
        acc[key] = e[key];
        return acc;
    }, {});
}
