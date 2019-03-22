import fixture from '../fixture.json';

import {
    Elm
} from './Main.elm';

for (let index = 0; index < 5; index++) {
    Elm.Main.init({
        flags: {
            data: fixture[ index ],
            id: index.toString()
        },
        node: document.getElementById(`chart-${index}`)
    });
}

