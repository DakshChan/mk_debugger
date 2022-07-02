import {useEffect, useLayoutEffect, useState} from "react";
import Prism from "./prism.js";
import "./prism.css"

export default function CodeContainer ({code, debug}) {

  const programPoint = () => {
    alert("test");
  };

  useLayoutEffect(() => {
    Prism.highlightAll();
  }, [code]);

  return (
    <div className="code-container">
      <pre><code className="language-racket">
          {code.split("\n").map((line, index) => {
            if (line.length > 50) {
              return <span key={index} style={{background: "red"}}>{line+"\n"}</span>
            } else {
              return <span key={index}>{line+"\n"}</span>;
            }
          })}
      </code></pre>
    </div>
  )
}
