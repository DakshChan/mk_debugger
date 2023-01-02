import { useEffect, useState } from "react";
import { CloseButton, Collapse, Divider, Heading, useDisclosure } from "@chakra-ui/react";

export default function PointInfoPanel({queries, pointDebug, code}) {
  const [state, setState] = useState(undefined);
  const { isOpen, onOpen, onClose } = useDisclosure()

  useEffect(() => {
    if (pointDebug !== undefined) {
      setState(pointDebug);
      onOpen();
    } else {
      setState(undefined);
      onClose();
    }
  }, [pointDebug, onClose, onOpen]);

  useEffect(() => {
    setState(undefined);
    onClose();
  }, [code, onClose]);

  return (
    <Collapse in={isOpen} animateOpacity className={"point-info-panel"}>
      <div style={{backgroundColor: "#f5f8fb", padding: "1em", borderRadius: "1em", width: "stretch",
      display: "flex", flexDirection: "column"}}>
        <div style={{display: "flex"}}>
          <Heading size={"md"} style={{flexGrow: "1"}}>Goal Statistics</Heading>
          <CloseButton onClick={onClose}/>
        </div>
        <Divider orientation='horizontal' borderColor={"grey"}/>
        <div style={{display: "flex", gap: "3ch"}}>
          <p style={{whiteSpace: "nowrap"}}>Line: {state?.syntax.line}</p>
          <p style={{whiteSpace: "nowrap"}}>Column: {state?.syntax.column}</p>
        </div>
        <div>
          <p style={{whiteSpace: "break-spaces"}}>Syntax: {state?.syntax.content}</p>
        </div>
        <div style={{display: "flex", gap: "3ch"}}>
          <p style={{whiteSpace: "nowrap"}}>Encounters: {state?.count}</p>
          <p style={{whiteSpace: "nowrap"}}>Failures: {state?.fails}</p>
          <p style={{whiteSpace: "nowrap"}}>Successes: {state?.successes}</p>
        </div>
      </div>
    </Collapse>
  );
}
