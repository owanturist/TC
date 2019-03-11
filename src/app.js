import fixture from '../fixture.json';

import {
    Elm
} from './Main.elm';

Elm.Main.init({
    flags: fixture[ 0 ]
});
