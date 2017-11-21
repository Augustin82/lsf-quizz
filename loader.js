module.exports = function(source) {
    var regex = /Asset\(\'(..\/static\/img\/hd\/[a-z]\.png)\'\)/g;
    var newSource = source.replace(regex, "Asset(require('$1'))");
    return newSource;
};
