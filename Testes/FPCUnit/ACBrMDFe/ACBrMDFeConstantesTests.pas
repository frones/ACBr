unit ACBrMDFeConstantesTests;

{$I ACBr.inc}

interface

const
  { Xmls de Envio }
  sXml_ConsSit = '<consSitMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                   '<tpAmb>2</tpAmb>' +
                   '<xServ>CONSULTAR</xServ>' +
                   '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                 '</consSitMDFe>';

  sXml_ConsNaoEnc = '<consMDFeNaoEnc xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                      '<tpAmb>2</tpAmb>' +
                      '<xServ>CONSULTAR NÃO ENCERRADOS</xServ>' +
                      '<CNPJ>12345678000123</CNPJ>' +
                    '</consMDFeNaoEnc>';

  sxml_EventoCancelamento = '<eventoMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                              '<infEvento Id="ID1101111234567890123456789012345678901234567890123401">' +
                                '<cOrgao>35</cOrgao>' +
                                '<tpAmb>2</tpAmb>' +
                                '<CNPJ>12345678000123</CNPJ>' +
                                '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                                '<dhEvento>2024-04-09T18:14:00</dhEvento>' +
                                '<tpEvento>110111</tpEvento>' +
                                '<nSeqEvento>1</nSeqEvento>' +
                                '<detEvento versaoEvento="4.00">' +
                                  '<evCancMDFe>' +
                                    '<descEvento>Cancelamento</descEvento>' +
                                    '<nProt>123456</nProt>' +
                                    '<xJust>Dados Errados Informados na Nota</xJust>' +
                                  '</evCancMDFe>' +
                                '</detEvento>' +
                              '</infEvento>' +
                            '</eventoMDFe>';

  sxml_EventoEncerramento = '<eventoMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                              '<infEvento Id="ID1101121234567890123456789012345678901234567890123401">' +
                                '<cOrgao>35</cOrgao>' +
                                '<tpAmb>2</tpAmb>' +
                                '<CNPJ>12345678000123</CNPJ>' +
                                '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                                '<dhEvento>2024-04-09T18:14:00</dhEvento>' +
                                '<tpEvento>110112</tpEvento>' +
                                '<nSeqEvento>1</nSeqEvento>' +
                                '<detEvento versaoEvento="4.00">' +
                                  '<evEncMDFe>' +
                                    '<descEvento>Encerramento</descEvento>' +
                                    '<nProt>123456</nProt>' +
                                    '<dtEnc>2024-04-09</dtEnc>' +
                                    '<cUF>35</cUF>' +
                                    '<cMun>3503208</cMun>' +
                                    '<indEncPorTerceiro>1</indEncPorTerceiro>' +
                                  '</evEncMDFe>' +
                                '</detEvento>' +
                              '</infEvento>' +
                            '</eventoMDFe>';

  sxml_EventoInclusaoCondutor = '<eventoMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                                  '<infEvento Id="ID1101141234567890123456789012345678901234567890123401">' +
                                    '<cOrgao>35</cOrgao>' +
                                    '<tpAmb>2</tpAmb>' +
                                    '<CNPJ>12345678000123</CNPJ>' +
                                    '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                                    '<dhEvento>2024-04-09T18:14:00</dhEvento>' +
                                    '<tpEvento>110114</tpEvento>' +
                                    '<nSeqEvento>1</nSeqEvento>' +
                                    '<detEvento versaoEvento="4.00">' +
                                      '<evIncCondutorMDFe>' +
                                        '<descEvento>Inclusao Condutor</descEvento>' +
                                        '<condutor>' +
                                          '<xNome>Pedro</xNome>' +
                                          '<CPF>12345678901</CPF>' +
                                        '</condutor>' +
                                      '</evIncCondutorMDFe>' +
                                    '</detEvento>' +
                                  '</infEvento>' +
                                '</eventoMDFe>';

  sxml_EventoInclusaoDFe = '<eventoMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                             '<infEvento Id="ID1101151234567890123456789012345678901234567890123401">' +
                               '<cOrgao>35</cOrgao>' +
                               '<tpAmb>2</tpAmb>' +
                               '<CNPJ>12345678000123</CNPJ>' +
                               '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                               '<dhEvento>2024-04-09T18:14:00</dhEvento>' +
                               '<tpEvento>110115</tpEvento>' +
                               '<nSeqEvento>1</nSeqEvento>' +
                               '<detEvento versaoEvento="4.00">' +
                                 '<evIncDFeMDFe>' +
                                   '<descEvento>Inclusao DF-e</descEvento>' +
                                   '<nProt>123456</nProt>' +
                                   '<cMunCarrega>3503208</cMunCarrega>' +
                                   '<xMunCarrega>Araraquara</xMunCarrega>' +
                                   '<infDoc>' +
                                     '<cMunDescarga>3503208</cMunDescarga>' +
                                     '<xMunDescarga>Araraquara</xMunDescarga>' +
                                     '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                   '</infDoc>' +
                                 '</evIncDFeMDFe>' +
                               '</detEvento>' +
                             '</infEvento>' +
                           '</eventoMDFe>';

  sxml_EventoPagamentoOperacao = '<eventoMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                                   '<infEvento Id="ID1101161234567890123456789012345678901234567890123401">' +
                                     '<cOrgao>35</cOrgao>' +
                                     '<tpAmb>2</tpAmb>' +
                                     '<CNPJ>12345678000123</CNPJ>' +
                                     '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                                     '<dhEvento>2024-04-09T18:14:00</dhEvento>' +
                                     '<tpEvento>110116</tpEvento>' +
                                     '<nSeqEvento>1</nSeqEvento>' +
                                     '<detEvento versaoEvento="4.00">' +
                                       '<evPagtoOperMDFe>' +
                                         '<descEvento>Pagamento Operacao MDF-e</descEvento>' +
                                         '<nProt>123456</nProt>' +
                                         '<infViagens>' +
                                           '<qtdViagens>00001</qtdViagens>' +
                                           '<nroViagem>00001</nroViagem>' +
                                         '</infViagens>' +
                                         '<infPag>' +
                                           '<xNome>Pedro</xNome>' +
                                           '<CNPJ>12345678901234</CNPJ>' +
                                           '<Comp>' +
                                             '<tpComp>01</tpComp>' +
                                             '<vComp>100.00</vComp>' +
                                             '<xComp>Comp1</xComp>' +
                                           '</Comp>' +
                                           '<vContrato>100.00</vContrato>' +
                                           '<indPag>0</indPag>' +
                                           '<infBanc>' +
                                             '<PIX>12345678901234</PIX>' +
                                           '</infBanc>' +
                                         '</infPag>' +
                                       '</evPagtoOperMDFe>' +
                                     '</detEvento>' +
                                   '</infEvento>' +
                                 '</eventoMDFe>';

  sxml_EventoAlteracaoPagtoServMDFe = '<eventoMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                                        '<infEvento Id="ID1101181234567890123456789012345678901234567890123401">' +
                                          '<cOrgao>35</cOrgao>' +
                                          '<tpAmb>2</tpAmb>' +
                                          '<CNPJ>12345678000123</CNPJ>' +
                                          '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                                          '<dhEvento>2024-04-09T18:14:00</dhEvento>' +
                                          '<tpEvento>110118</tpEvento>' +
                                          '<nSeqEvento>1</nSeqEvento>' +
                                          '<detEvento versaoEvento="4.00">' +
                                            '<evAlteracaoPagtoServMDFe>' +
                                              '<descEvento>Alteracao Pagamento Servico MDFe</descEvento>' +
                                              '<nProt>123456</nProt>' +
                                              '<infPag>' +
                                                '<xNome>Pedro</xNome>' +
                                                '<CNPJ>12345678901234</CNPJ>' +
                                                '<Comp>' +
                                                  '<tpComp>01</tpComp>' +
                                                  '<vComp>100.00</vComp>' +
                                                  '<xComp>Comp1</xComp>' +
                                                '</Comp>' +
                                                '<vContrato>100.00</vContrato>' +
                                                '<indPag>0</indPag>' +
                                                '<infBanc>' +
                                                  '<PIX>12345678901234</PIX>' +
                                                '</infBanc>' +
                                              '</infPag>' +
                                            '</evAlteracaoPagtoServMDFe>' +
                                          '</detEvento>' +
                                        '</infEvento>' +
                                      '</eventoMDFe>';

  sxml_EventoConfirmaServMDFe = '<eventoMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                                  '<infEvento Id="ID1101171234567890123456789012345678901234567890123401">' +
                                    '<cOrgao>35</cOrgao>' +
                                    '<tpAmb>2</tpAmb>' +
                                    '<CNPJ>12345678000123</CNPJ>' +
                                    '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                                    '<dhEvento>2024-04-09T18:14:00</dhEvento>' +
                                    '<tpEvento>110117</tpEvento>' +
                                    '<nSeqEvento>1</nSeqEvento>' +
                                    '<detEvento versaoEvento="4.00">' +
                                    '<evConfirmaServMDFe>' +
                                      '<descEvento>Confirmacao Servico Transporte</descEvento>' +
                                      '<nProt>123456</nProt>' +
                                    '</evConfirmaServMDFe>' +
                                    '</detEvento>' +
                                  '</infEvento>' +
                                '</eventoMDFe>';

  { Xmls de Retorno }
  sxml_Ret_Vazio =  '';

  sxml_RetConsSit = '<retConsSitMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                      '<tpAmb>2</tpAmb>' +
                      '<verAplic>SP_PL_MDFe_400</verAplic>' +
                      '<cStat>100</cStat>' +
                      '<xMotivo>Autorizado o uso do MDF-e</xMotivo>' +
                      '<cUF>35</cUF>' +
                      '<protMDFe versao="4.00">' +
                        '<infProt>' +
                          '<tpAmb>2</tpAmb>' +
                          '<verAplic>SP_PL_MDFe_400</verAplic>' +
                          '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                          '<dhRecbto>2010-08-31T19:20:22</dhRecbto>' +
                          '<nProt>123456789012345</nProt>' +
                          '<digVal>PNG7OJ2WYLQyhkL2kWBykEGSVQA=</digVal>' +
                          '<cStat>100</cStat>' +
                          '<xMotivo>Autorizado o uso do MDF-e</xMotivo>' +
                          '<infFisco>' +
                            '<cMsg>1</cMsg>' +
                            '<xMsg>Autorizado</xMsg>' +
                          '</infFisco>' +
                        '</infProt>' +
                      '</protMDFe>' +

                      '<procEventoMDFe versao="4.00">' +

                        '<eventoMDFe versao="4.00">' +
                          '<infEvento Id="ID1101111234567890123456789012345678901234567890123401">' +
                            '<cOrgao>35</cOrgao>' +
                            '<tpAmb>2</tpAmb>' +
                            '<CNPJ>12345678000123</CNPJ>' +
                            '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                            '<dhEvento>2024-04-09T18:14:00</dhEvento>' +
                            '<tpEvento>110111</tpEvento>' +
                            '<nSeqEvento>1</nSeqEvento>' +
                            '<detEvento versaoEvento="4.00">' +
                              '<evCancMDFe>' +
                                '<descEvento>Cancelamento</descEvento>' +
                                '<nProt>123456</nProt>' +
                                '<xJust>Dados Errados Informados na Nota</xJust>' +
                              '</evCancMDFe>' +
                            '</detEvento>' +
                          '</infEvento>' +
                        '</eventoMDFe>' +

                        '<retEventoMDFe versao="4.00">' +
                          '<infEvento>' +
                            '<tpAmb>1</tpAmb>' +
                            '<verAplic>SP_PL_MDFe_400</verAplic>' +
                            '<cOrgao>35</cOrgao>' +
                            '<cStat>135</cStat>' +
                            '<xMotivo>Evento registrado e vinculado ao MDF-e</xMotivo>' +
                            '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                            '<tpEvento>110111</tpEvento>' +
                            '<xEvento>Cancelamento registrado</xEvento>' +
                            '<nSeqEvento>1</nSeqEvento>' +
                            '<dhRegEvento>2018-09-18T16:07:56-03:00</dhRegEvento>' +
                            '<nProt>123456789012345</nProt>' +
                          '</infEvento>' +
                        '</retEventoMDFe>' +
                      '</procEventoMDFe>' +
                    '</retConsSitMDFe>';

  sxml_RetConsNaoEnc = '<retConsMDFeNaoEnc xmlns="http://www.portalfiscal.inf.br/mdfe" versao="4.00">' +
                         '<tpAmb>2</tpAmb>' +
                         '<verAplic>SP_PL_MDFe_400</verAplic>' +
                         '<cStat>100</cStat>' +
                         '<xMotivo>MDFe nao encerrado</xMotivo>' +
                         '<cUF>35</cUF>' +
                         '<infMDFe>' +
                           '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                           '<nProt>123456789012345</nProt>' +
                         '</infMDFe>' +
                       '</retConsMDFeNaoEnc>';

  sxml_RetEnvEvento =  '<retEventoMDFe versao="4.00">' +
                         '<infEvento>' +
                           '<tpAmb>2</tpAmb>' +
                           '<verAplic>SP-MDFe-2023-10-05-1</verAplic>' +
                           '<cOrgao>35</cOrgao>' +
                           '<cStat>135</cStat>' +
                           '<xMotivo>Evento registrado</xMotivo>' +
                           '<chMDFe>12345678901234567890123456789012345678901234</chMDFe>' +
                           '<tpEvento>110111</tpEvento>' +
                           '<xEvento>Cancelamento</xEvento>' +
                           '<nSeqEvento>1</nSeqEvento>' +
                           '<dhRegEvento>2023-10-23T17:15:05-03:00</dhRegEvento>' +
                           '<nProt>123456789012345</nProt>' +
                         '</infEvento>' +
                       '</retEventoMDFe>';

implementation

end.
