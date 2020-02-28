import * as React from "react";
import * as ReactDOM from "react-dom";

import { Hello } from "./components/Hello";

export function createUI(elementSelector: string) {
  ReactDOM.render(
    <Hello compiler="TypeScript" framework="React" />,
    document.querySelector(elementSelector)
  );
}

