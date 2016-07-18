console.log("Running phantomjs");

var page = require('webpage').create();

page.onConsoleMessage = function (message) {
    console.log(message);
};

page.open('site/public/index.html', function (status) {
    try {
        var failed = page.evaluate(function () {
            return reagenttest.runtests.failed();
        });
        phantom.exit(failed);
    } catch (e) {
        console.error("Error: ", e);
        phantom.exit(1);
    }
});
