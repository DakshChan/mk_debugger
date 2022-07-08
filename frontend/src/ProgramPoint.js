import {useEffect, useState, memo} from "react";

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
      let c = Math.floor(100 - (Math.pow(-1 *
        rangeMap(data.count, range.encounters.min, range.encounters.max, 0, 1) + 1, 2.2)) * 100);
      let c2 = Math.floor(rangeMap(c, 0, 100, 15, 70));
      setColor(`rgba(255, 0, 0, ${c2}%)`);
    } else if (codeHighlight.info === "failures") {
      let c = Math.floor(100 - (Math.pow(-1 *
        rangeMap(data.fails, range.failures.min, range.failures.max, 0, 1) + 1, 2.2)) * 100);
      let c2 = Math.floor(rangeMap(c, 0, 100, 15, 70));
      setColor(`rgba(255, 0, 0, ${c2}%)`);
    } else {
      setColor(`rgba(0, 0, 0, 0%)`);
    }
  }

  useEffect(() => {
    colorMap();
  }, [range, codeHighlight]);

  // if (codeHighlight.style === "color") {
  return (
    <>
      <span className="program-point" onClick={() => setPointDebug(data)} style={{background: color, borderRadius: "0.3em", outlineStyle:"solid", outlineColor: color, outlineWidth:"0.1em"}}>
        {children}
      </span>
    </>
  );
  // } else if (codeHighlight.style === "bars") {
  //   return (
  //     <>
  //         <span className="program-point" onClick={() => setPointDebug(data)} style={{background: color, borderRadius: "1em", outlineStyle:"solid", outlineColor: color, outlineWidth:"0.1em"}}>
  //           {children}
  //         </span>
  //     </>
  //   );
  // } else {
  //   return (
  //     <>
  //         <span className="program-point" onClick={() => setPointDebug(data)} style={{borderRadius: "0.3em", outlineStyle:"solid", outlineColor: "#000", outlineWidth:"0.1em"}}>
  //           {children}
  //         </span>
  //     </>
  //   );
  // }
}
