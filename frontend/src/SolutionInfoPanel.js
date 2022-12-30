import "./InfoPanel.css"
import React, {useEffect, useState} from "react";
import {
  Accordion, AccordionButton, AccordionIcon, AccordionItem, AccordionPanel,
  Button, Box, Heading, Input, useDisclosure,
  Table, TableContainer, Tbody, Td, Th, Thead, Tr,
  Drawer, DrawerBody, DrawerFooter, DrawerHeader, DrawerOverlay, DrawerContent, DrawerCloseButton, Code, Select,
} from "@chakra-ui/react";

export default function SolutionInfoPanel({queries, code}) {
  const [state, setState] = useState(undefined);
  const [solution, setSolution] = useState(undefined);
  const [debug, setDebug] = useState(undefined);

  const { isOpen, onOpen, onClose } = useDisclosure()

  useEffect(() => {
    if (debug !== undefined) {
      setSolution(debug.solutions);
    } else {
      setSolution(undefined);
    }
    setState(undefined);
  }, [debug]);

  useEffect(() => {
    setState(undefined);
    setSolution(undefined);
  }, [code]);

  return (
    <div className={"solution-info-panel"}>
      <Drawer isOpen={isOpen} placement='right' onClose={onClose} size={"md"}>
        <DrawerContent>
          <DrawerCloseButton />
          <DrawerHeader>Path</DrawerHeader>
          <DrawerBody>
            <Accordion defaultIndex={[0]} allowMultiple>
              {
                (state === undefined || state.path === undefined || state.path.length === 0) ? <></> :
                <AccordionItem>
                  <h2>
                    <AccordionButton>
                      <Box as="span" flex='1' textAlign='left'>
                        Path
                      </Box>
                      <AccordionIcon />
                    </AccordionButton>
                  </h2>
                  <AccordionPanel style={{display: "flex", flexDirection: "column", gap:"0.2em", alignItems: "baseline"}}>
                    <Heading size={"sm"}>{state.path.length}</Heading>
                    {state.path.slice(0).reverse().map((s, index) => {
                      return <Code key={index}>{`${s.line !== false ? s.line + ":" + s.column : ""} ${s.content}`}</Code>;
                    })}
                  </AccordionPanel>
                </AccordionItem>
              }
              {
                (state === undefined || state.stack === undefined || state.stack.length === 0) ? <></> :
                <AccordionItem>
                  <h2>
                    <AccordionButton>
                      <Box as="span" flex='1' textAlign='left'>
                        Stack
                      </Box>
                      <AccordionIcon />
                    </AccordionButton>
                  </h2>
                  <AccordionPanel style={{display: "flex", flexDirection: "column", gap:"0.2em", alignItems: "baseline"}}>
                    <Heading size={"sm"}>{state.stack.length}</Heading>
                    {state.stack.slice(0).reverse().map((s, index) => {
                      return <Code key={index}>{`${s.line !== false ? s.line + ":" + s.column : ""} ${s.content}`}</Code>;
                    })}
                  </AccordionPanel>
              </AccordionItem>
              }
            </Accordion>
          </DrawerBody>
        </DrawerContent>
      </Drawer>
      <Heading size={"md"}>Solutions {(solution !== undefined ? `- ${solution.length}` : "")}</Heading>
      { (Object.keys(queries).length > 0) ?
        <Select width={"unset"} size={"sm"} defaultValue={""} onChange={(event) => setDebug(queries[event.target.value])}>
          {
            Object.keys(queries).map((q) => {
              return <option value={q}>{q}</option>
            })
          }
        </Select> : <></>
      }
      {
        (solution !== undefined) ?
        <TableContainer overflowY={"auto"} maxHeight={`${5 * 3 + 1.5}em`}
                        scrollSnapType={"y mandatory"} style={{scrollPaddingBlockStart: "1.5em"}}>
          <Table variant={"striped"} size={"sm"} colorScheme={"green"}>
            <Thead position={"sticky"} top={"0"} style={{backgroundColor: "#f5f8fb", zIndex: "1"}}>
              <Tr>
                <Th>Terms</Th>
                <Th>Constraints</Th>
                <Th>Path</Th>
              </Tr>
            </Thead>
            <Tbody>
              {
                solution.map((s, i) => {
                  return (
                    <Tr key={i} scrollSnapAlign={"start"}>
                      <Td style={{display: "flex", alignItems:"center", gap:"2ch"}}>
                        <p style={{fontWeight: "bolder", fontSize:"150%"}}>{i}</p>
                        <div>
                          {
                            Object.entries(s.binding.sub).map(([key, val]) => {
                              return (
                                <div style={{display: "flex", alignItems: "center", gap:"1ch"}}>
                                  <p style={{fontWeight: "bold"}}>{key}:</p>
                                  <p style={{whiteSpace: "break-spaces"}}>{val}</p>
                                </div>
                              );
                            })
                          }
                        </div>
                      </Td>
                      <Td><p style={{whiteSpace: "break-spaces"}}>{s.binding.cxs || "N/A"}</p></Td>
                      <Td><Button size={"sm"} style={{zIndex: 0}} onClick={() => {setState(s); onOpen()}}>Path</Button></Td>
                    </Tr>
                  )
                })
              }
            </Tbody>
          </Table>
        </TableContainer>
          : <p>Run a query</p>
      }
    </div>
  );
}
