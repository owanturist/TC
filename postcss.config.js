const import_ = require('postcss-import');
const customProperties = require('postcss-custom-properties');
const calc = require('postcss-calc');
const nested = require('postcss-nested');
const autoprefixer = require('autoprefixer');


module.exports = {
    plugins: [
        import_(),
        customProperties(),
        calc({
            precision: 2
        }),
        nested(),
        autoprefixer()
    ]
};
