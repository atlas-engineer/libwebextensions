// File created for a more convenient iteration on Promise callback
// (see make-jsc-promise in webextensions.scm)

function closure (check) {
    function rec (success, failure) {
        var result = check();
        if (result === null) {
            setTimeout(() => {
                rec(success, failure);
            },
                       100);
        } else {
            if (result.hasOwnProperty("error")) {
                let error = new Error(value.error);
                failure(error);
            } else if (value.hasOwnProperty("result")) {
                success(value.result);
            }
        }
    }
    return new Promise(rec);
} closure
