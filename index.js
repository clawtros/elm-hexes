import './src/main.css';
import { Elm } from './src/Main.elm';

window.initHexes = function(element) {
  Elm.Main.init({
    node: element
  });  
}
