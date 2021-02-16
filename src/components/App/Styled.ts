import { createGlobalStyle } from "styled-components";
import { normalize } from "polished";

export const SApp = createGlobalStyle`
  ${normalize()}

  body {
    font-family: "72", sans-serif;
    font-size: 16px;
  }
`;
