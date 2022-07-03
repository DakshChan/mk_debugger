import {useEffect, useLayoutEffect, useState} from "react";
import Prism from "./prism.js";
import "./prism.css"
import ProgramPoint from "./ProgramPoint";

export default function CodeContainer ({code, debug}) {
  const [parsedLines, setParsedLines] = useState([""]);

  useEffect(() => {
    setParsedLines([code]);
  }, [code]);

  useEffect(() => {
    setParsedLines(programPointParser());
  }, [debug]);

  useLayoutEffect(() => {
    Prism.highlightAll();
  }, [parsedLines]);

  const programPointParser = () => {
    let resList = [];
    if (debug !== undefined) {
      let programPointList = debug["program-points"];
      console.log(programPointList);
      console.log(code.length);
      programPointList.sort((a, b) =>  a.syntax.position - b.syntax.position);
      let end = 1, start = 1;
      for (const programPoint of programPointList) {
        console.log(programPoint);
        start = programPoint.syntax.position;
        console.log(end);
        if (start - end > 0) {
          resList.push({"key": end, "text": code.substring(start-1, end-1), "data": undefined});
        }
        end = start + programPoint.syntax.span;
        console.log(start);
        //<span key={start} style={{background: "red"}}>{code.substring(start, end)}</span>
        resList.push({"key": start, "text": code.substring(start-1, end-1), "data": programPoint});
      }
      if (end !== code.length) {
        resList.push({"key": end, "text": code.substring(end, code.length), "data": undefined});
      }

    }
    console.log(resList);
    return resList
  }

  return (
    <div className="code-container">
      <pre><code className="language-racket">
          {
            parsedLines.map((line, index) => {
              if (line.data !== undefined) {
                return <ProgramPoint key={index} style={{background: "red"}} data={line.data}>{line.text}</ProgramPoint>
              } else {
                return <span key={index}>{line.text}</span>
              }
            })
          }
      </code></pre>
    </div>
  )
}
