// File created for a more convenient iteration on Promise callback
// (see make-jsc-promise in webextensions.scm)

function closure (check) {
    function rec (success, failure) {
        var value = check();
        console.log("Got " + JSON.stringify(value) + " value");
        if (result === null) {
            setTimeout(() => {
                console.log("Timeout fired");
                rec(success, failure);
            },
                       100);
        } else {
            if (result.hasOwnProperty("error")) {
                let error = new Error(value.error);
                failure(error);
            } else if (value.hasOwnProperty("results")) {
                success(...value.result);
            } else {
                let mismatch = new Error("Value passed to Promise callback is malformed: "
                                       + JSON.stringify(value)
                                       + " and missing results/error field.");
                failure(mismatch);
            }
        }
    }
    return new Promise(rec);
} closure
