import React from "react";
import { RouteComponentProps } from "react-router-dom";

interface Props extends RouteComponentProps {}

export const Home: React.FC<Props> = () => {
  return (
    <div>
      <h1>Home</h1>
    </div>
  );
};
