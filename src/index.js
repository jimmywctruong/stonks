'use strict';

require('./index.html');
const {Elm} = require('./Main.elm');

document.addEventListener('DOMContentLoaded', function() {
    Elm.Main.init({node: document.getElementById('main')});
});
