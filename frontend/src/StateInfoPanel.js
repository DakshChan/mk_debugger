import { useEffect, useState } from "react";
import {
  Accordion, AccordionButton, AccordionIcon, AccordionItem, AccordionPanel,
  Button, Box, Heading, useDisclosure, Code, Select, Divider,
  Table, TableContainer, Tbody, Td, Th, Thead, Tr,
  Drawer, DrawerBody, DrawerHeader, DrawerContent, DrawerCloseButton,
} from "@chakra-ui/react";

export default function StateInfoPanel({queries, code}) {
  const [state, setState] = useState(undefined);
  const [solution, setSolution] = useState(undefined);
  const [rejection, setRejection] = useState(undefined);
  const [debug, setDebug] = useState(undefined);

  const { isOpen, onOpen, onClose } = useDisclosure()

  useEffect(() => {
    if (debug !== undefined) {
      setSolution(debug.solutions);
      setRejection(debug["rejected-states"]);
    } else {
      setSolution(undefined);
      setRejection(undefined);
    }
    setState(undefined);
  }, [debug]);

  useEffect(() => {
    setState(undefined);
    setSolution(undefined);
    setRejection(undefined);
  }, [code]);

  useEffect(() => {
    if (Object.keys(queries).length > 0) {
      setDebug(queries[(Object.keys(queries)[0])]);
    }
  }, [queries]);

  return (
    <div style={{backgroundColor: "#f5f8fb",
      padding: "1em", borderRadius: "1em", width: "stretch",
      display: "flex", flexDirection: "column", gap: "1em"}}>
      <Drawer isOpen={isOpen} placement='right' onClose={onClose} size={"md"}>
        <DrawerContent>
          <DrawerCloseButton />
          <DrawerHeader>
            {(state !== undefined && state.stack !== undefined && state.stack.length > 0) ? "Path/Stack" : "Path"}
          </DrawerHeader>
          <DrawerBody>
            <Accordion defaultIndex={[0, 1]} allowMultiple>
              {
                (state === undefined || state.path === undefined || state.path.length === 0) ? <></> :
                <AccordionItem key={'path'}>
                  <AccordionButton>
                    <Box as="span" flex='1' textAlign='left'>
                      Path
                    </Box>
                    <AccordionIcon />
                  </AccordionButton>
                  <AccordionPanel style={{display: "flex", flexDirection: "column", gap:"0.2em", alignItems: "baseline"}}>
                    <Table size='sm' variant='unstyled' width='unset'>
                      <Thead>
                        <Tr>
                          <Th padding={'0'} paddingInline={'0'}></Th>
                          <Th padding={'0'} paddingInline={'0'}></Th>
                          <Th padding={'0'} paddingInline={'0'}></Th>
                          <Th padding={'0'} paddingInline={'0'}></Th>
                        </Tr>
                      </Thead>
                      <Tbody>
                        {state.path.slice(0).reverse().map((s, index) => {
                          return (
                            <Tr key={index}>
                              <Td padding={'0'} paddingInline={'0'}>
                                {s.line !== false ? s.line : ""}
                              </Td>
                              <Td padding={'0'} paddingInline={'0.5ch'}>
                                {s.line !== false ? ":" : ""}
                              </Td>
                              <Td padding={'0'} paddingInline={''}>
                                {s.line !== false ? s.column : ""}
                              </Td>
                              <Td padding={'0'} paddingInline={'1ch'}>
                                <Code>{s.content}</Code>
                              </Td>
                            </Tr>
                          );
                        })}
                      </Tbody>
                    </Table>
                  </AccordionPanel>
                </AccordionItem>
              }
              {
                (state === undefined || state.stack === undefined || state.stack.length === 0) ? <></> :
                <AccordionItem key={'stack'}>
                  <AccordionButton>
                    <Box as="span" flex='1' textAlign='left'>
                      Stack
                    </Box>
                    <AccordionIcon />
                  </AccordionButton>
                  <AccordionPanel style={{display: "flex", flexDirection: "column", gap:"0.2em", alignItems: "baseline"}}>
                    <Table size='sm' variant='unstyled' width='unset'>
                      <Thead>
                        <Tr>
                          <Th padding={'0'} paddingInline={'0'}></Th>
                          <Th padding={'0'} paddingInline={'0'}></Th>
                          <Th padding={'0'} paddingInline={'0'}></Th>
                          <Th padding={'0'} paddingInline={'0'}></Th>
                        </Tr>
                      </Thead>
                      <Tbody>
                        {state.stack.slice(0).reverse().map((s, index) => {
                          return (
                            <Tr key={index}>
                              <Td padding={'0'} paddingInline={'0'}>
                                {s.line !== false ? s.line : ""}
                              </Td>
                              <Td padding={'0'} paddingInline={'0.5ch'}>
                                {s.line !== false ? ":" : ""}
                              </Td>
                              <Td padding={'0'} paddingInline={''}>
                                {s.line !== false ? s.column : ""}
                              </Td>
                              <Td padding={'0'} paddingInline={'1ch'}>
                                <Code>{s.content}</Code>
                              </Td>
                            </Tr>
                          );
                        })}
                      </Tbody>
                    </Table>
                  </AccordionPanel>
              </AccordionItem>
              }
            </Accordion>
          </DrawerBody>
        </DrawerContent>
      </Drawer>
      <div style={{display: "flex", gap: "1ch", alignItems:"center"}}>
        <Heading size={"md"}>Query</Heading>
        { (Object.keys(queries).length > 0) ?
          <Select width={"unset"} size={"sm"} backgroundColor={"white"}
                  onChange={(event) => setDebug(queries[event.target.value])}>
            {
              Object.keys(queries).map((q, key) => {
                return (<option key={key} value={q}>{parseInt(q) + 1}</option>);
              })
            }
          </Select> : <></>
        }
      </div>
      <Divider orientation='horizontal' borderColor={"grey"}/>
      <Heading size={"md"}>Answers {(solution !== undefined ? `: ${solution.length} Total` : "")}</Heading>
      {
        (solution !== undefined) ?
        <TableContainer overflowY={"auto"} maxHeight={`${5 * 3}em`}
                        scrollSnapType={"y mandatory"} style={{scrollPaddingBlockStart: "1.5em"}}>
          <Table variant={"striped"} size={"sm"} colorScheme={"green"}>
            <Thead position={"sticky"} top={"0"} style={{backgroundColor: "#f5f8fb", zIndex: "1"}}>
              <Tr>
                <Th>Terms</Th>
                <Th>Constraints</Th>
                <Th isNumeric>Path</Th>
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
                                <div key={key} style={{display: "flex", alignItems: "center", gap:"1ch"}}>
                                  <p style={{fontWeight: "bold"}}>{key}:</p>
                                  <p style={{whiteSpace: "break-spaces"}}>{val}</p>
                                </div>
                              );
                            })
                          }
                        </div>
                      </Td>
                      <Td><p style={{whiteSpace: "break-spaces"}}>{s.binding.cxs}</p></Td>
                      <Td isNumeric><Button size={"sm"} style={{zIndex: 0}} onClick={() => {setState(s); onOpen()}}>Path</Button></Td>
                    </Tr>
                  )
                })
              }
            </Tbody>
          </Table>
        </TableContainer>
          : <p>Run a query</p>
      }
      <Divider orientation='horizontal' borderColor={"grey"}/>
      <Heading size={"md"}>Rejected States {(rejection !== undefined ? `: ${rejection.length} Random samples` : "")}</Heading>
      {
        (rejection !== undefined) ?
        <TableContainer overflowY={"auto"} maxHeight={`${5 * 3}em`}
                        scrollSnapType={"y mandatory"} style={{scrollPaddingBlockStart: "1.5em"}}>
          <Table variant={"striped"} size={"sm"} colorScheme={"red"}>
            <Thead position={"sticky"} top={"0"} style={{backgroundColor: "#f5f8fb", zIndex: "1"}}>
              <Tr>
                <Th>Terms</Th>
                <Th>Constraints</Th>
                <Th>Failed Goal</Th>
                <Th>Failed Constraints</Th>
                <Th isNumeric>Path/Stack</Th>
              </Tr>
            </Thead>
            <Tbody>
              {
                rejection.map((s, i) => {
                  return (
                    <Tr key={i} scrollSnapAlign={"start"}>
                      <Td style={{display: "flex", alignItems:"center", gap:"2ch"}}>
                        <p style={{fontWeight: "bolder", fontSize:"150%"}}>{i} </p>
                        <div>
                          {
                            Object.entries(s.binding.sub).map(([key, val]) => {
                              return (
                                <div key={key} style={{display: "flex", alignItems: "center", gap:"1ch"}}>
                                  <p style={{fontWeight: "bold"}}>{key}:</p>
                                  <p style={{whiteSpace: "break-spaces"}}>{val}</p>
                                </div>
                              );
                            })
                          }
                        </div>
                      </Td>
                      <Td><p style={{whiteSpace: "break-spaces"}}>{s.binding.cxs}</p></Td>
                      <Td><p style={{whiteSpace: "break-spaces"}}>{s.failed.sub}</p></Td>
                      <Td><p style={{whiteSpace: "break-spaces"}}>{s.failed.cxs}</p></Td>
                      <Td isNumeric><Button size={"sm"} style={{zIndex: 0}} onClick={() => {setState(s); onOpen()}}>Path/Stack</Button></Td>
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