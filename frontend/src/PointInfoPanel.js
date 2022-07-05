import {useEffect} from "react";

export default function PointInfoPanel({pointDebug}) {

  useEffect(() => {
      console.log(pointDebug);
    }, [pointDebug])

  return (
    <>{JSON.stringify(pointDebug)}</>
  );
}
