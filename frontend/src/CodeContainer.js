import { useEffect } from "react";
import Prism from "./prism.js";
import "./prism.css"

export default function CodeContainer () {

  useEffect(() => {
    Prism.highlightAll();
  }, []);

  const codePoint = () => {
    alert("test");
  }

  return (
    <div className="code-container">
      <pre>
        <code className="language-racket line-numbers">
          <span onMouseEnter={codePoint}>(require racket/syntax)</span>
          {'\n'}
          (syntax (+ 2 (- 2 3)))
          asdf
          a
          sdffffffffd
        </code>
      </pre>
    </div>
  )
}
