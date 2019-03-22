import {
    Elm
} from './Main.elm';

for (var index = 0; index < 5; index++) {
    Elm.Main.init({
        flags: {
            data: window.__fixture[ index ],
            id: index.toString()
        },
        node: document.getElementById('chart-' + index.toString())
    });
}

