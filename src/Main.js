exports.createObjectURL = function(file) {
  return function() {
    return URL.createObjectURL(file);
  };
};

exports.revokeObjectURL = function(objectURL) {
  return function() {
    return URL.revokeObjectURL(objectURL);
  };
};
