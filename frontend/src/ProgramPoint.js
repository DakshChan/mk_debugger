import {useEffect, useState} from "react";

export default function ProgramPoint({children, data, range, codeHighlight, setPointDebug}) {

  const [color, setColor] = useState("argb(255, 0, 0, 100)");

  function rangeMap(x, in_min, in_max, out_min, out_max) {
    if (in_min === in_max) {
      return out_max;
    } else {
      return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
    }
  }

  function colorMap() {
    if (codeHighlight.info === "encounters") {
      let c = Math.floor(rangeMap(data.count, range.min, range.max, 0, 255));
      setColor(`rgba(${c}, 0, ${255 - c}, 40%)`);
    } else if (codeHighlight.info === "failures") {
      //tbd when rejection data is available
      setColor(`rgba(255, 0, 0, 40%)`);
    } else {
      setColor(`rgba(0, 0, 0, 0%)`);
    }
  }

  useEffect(() => {
    colorMap();
  }, [range, codeHighlight]);

  return (
    <>
      <span className="program-point" onClick={() => setPointDebug(data)} style={{background: color, borderRadius: "0.3em", outlineStyle:"solid", outlineColor: color, outlineWidth:"0.1em"}}>
        {children}
      </span>
    </>
  );
}
