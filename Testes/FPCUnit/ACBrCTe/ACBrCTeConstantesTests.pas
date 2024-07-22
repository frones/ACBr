unit ACBrCTeConstantesTests;

{$I ACBr.inc}

interface

const
  { Xmls de Envio }
  sXml_ConsSit = '<consSitCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                   '<tpAmb>2</tpAmb>' +
                   '<xServ>CONSULTAR</xServ>' +
                   '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                 '</consSitCTe>';

  sxml_EventoCCe = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                     '<infEvento Id="ID11011012345678901234567890123456789012345678901234001">' +
                       '<cOrgao>35</cOrgao>' +
                       '<tpAmb>2</tpAmb>' +
                       '<CNPJ>12345678000123</CNPJ>' +
                       '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                       '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                       '<tpEvento>110110</tpEvento>' +
                       '<nSeqEvento>1</nSeqEvento>' +
                       '<detEvento versaoEvento="4.00">' +
                         '<evCCeCTe>' +
                           '<descEvento>Carta de Correcao</descEvento>' +
                           '<infCorrecao>' +
                             '<grupoAlterado>grupo</grupoAlterado>' +
                             '<campoAlterado>campo</campoAlterado>' +
                             '<valorAlterado>valor</valorAlterado>' +
                             '<nroItemAlterado>01</nroItemAlterado>' +
                           '</infCorrecao>' +
                           '<xCondUso>A Carta de Correcao e disciplinada pelo Art. 58-B do ' +
                             'CONVENIO/SINIEF 06/89: Fica permitida a utilizacao de carta de ' +
                             'correcao, para regularizacao de erro ocorrido na emissao de ' +
                             'documentos fiscais relativos a prestacao de servico de transporte, ' +
                             'desde que o erro nao esteja relacionado com: I - as variaveis que ' +
                             'determinam o valor do imposto tais como: base de calculo, aliquota, ' +
                             'diferenca de preco, quantidade, valor da prestacao;II - a correcao ' +
                             'de dados cadastrais que implique mudanca do emitente, tomador, ' +
                             'remetente ou do destinatario;III - a data de emissao ou de saida.</xCondUso>' +
                         '</evCCeCTe>' +
                       '</detEvento>' +
                     '</infEvento>' +
                   '</eventoCTe>';

  sxml_EventoCancelamento = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                              '<infEvento Id="ID11011112345678901234567890123456789012345678901234001">' +
                                '<cOrgao>35</cOrgao>' +
                                '<tpAmb>2</tpAmb>' +
                                '<CNPJ>12345678000123</CNPJ>' +
                                '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                                '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                '<tpEvento>110111</tpEvento>' +
                                '<nSeqEvento>1</nSeqEvento>' +
                                '<detEvento versaoEvento="4.00">' +
                                  '<evCancCTe>' +
                                    '<descEvento>Cancelamento</descEvento>' +
                                    '<nProt>123456</nProt>' +
                                    '<xJust>Dados Errados Informados na Nota</xJust>' +
                                  '</evCancCTe>' +
                                '</detEvento>' +
                              '</infEvento>' +
                            '</eventoCTe>';

  sxml_EventoEPEC = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                      '<infEvento Id="ID11011312345678901234567890123456789012345678901234001">' +
                        '<cOrgao>35</cOrgao>' +
                        '<tpAmb>2</tpAmb>' +
                        '<CNPJ>12345678000123</CNPJ>' +
                        '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                        '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                        '<tpEvento>110113</tpEvento>' +
                        '<nSeqEvento>1</nSeqEvento>' +
                        '<detEvento versaoEvento="4.00">' +
                          '<evEPECCTe>' +
                            '<descEvento>EPEC</descEvento>' +
                            '<xJust>Produto diferente do pedido</xJust>' +
                            '<vICMS>10.00</vICMS>' +
                            '<vICMSST>10.00</vICMSST>' +
                            '<vTPrest>10.00</vTPrest>' +
                            '<vCarga>10.00</vCarga>' +
                            '<toma4>' +
                              '<toma>0</toma>' +
                              '<UF>SP</UF>' +
                              '<CNPJ>12345678000123</CNPJ>' +
                              '<IE>12345</IE>' +
                            '</toma4>' +
                            '<modal>01</modal>' +
                            '<UFIni>SP</UFIni>' +
                            '<UFFim>SP</UFFim>' +
                            '<tpCTe>0</tpCTe>' +
                            '<dhEmi>2024-04-09T18:14:00-03:00</dhEmi>' +
                          '</evEPECCTe>' +
                        '</detEvento>' +
                      '</infEvento>' +
                    '</eventoCTe>';

  sxml_EventoMultModal = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                          '<infEvento Id="ID11016012345678901234567890123456789012345678901234001">' +
                            '<cOrgao>35</cOrgao>' +
                            '<tpAmb>2</tpAmb>' +
                            '<CNPJ>12345678000123</CNPJ>' +
                            '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                            '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                            '<tpEvento>110160</tpEvento>' +
                            '<nSeqEvento>1</nSeqEvento>' +
                            '<detEvento versaoEvento="4.00">' +
                              '<evRegMultimodal>' +
                                '<descEvento>Registro Multimodal</descEvento>' +
                                '<xRegistro>Registro</xRegistro>' +
                                '<nDoc>10</nDoc>' +
                              '</evRegMultimodal>' +
                            '</detEvento>' +
                          '</infEvento>' +
                         '</eventoCTe>';

  sxml_EventoPrestacaoDesacordo = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                                    '<infEvento Id="ID61011012345678901234567890123456789012345678901234001">' +
                                      '<cOrgao>35</cOrgao>' +
                                      '<tpAmb>2</tpAmb>' +
                                      '<CNPJ>12345678000123</CNPJ>' +
                                      '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                                      '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                      '<tpEvento>610110</tpEvento>' +
                                      '<nSeqEvento>1</nSeqEvento>' +
                                      '<detEvento versaoEvento="4.00">' +
                                        '<evPrestDesacordo>' +
                                          '<descEvento>Prestacao do Servico em Desacordo</descEvento>' +
                                          '<indDesacordoOper>1</indDesacordoOper>' +
                                          '<xObs>motivo do desacordo</xObs>' +
                                        '</evPrestDesacordo>' +
                                      '</detEvento>' +
                                    '</infEvento>' +
                                  '</eventoCTe>';

  sxml_EventoCancPrestacaoDesacordo = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                                        '<infEvento Id="ID61011112345678901234567890123456789012345678901234001">' +
                                          '<cOrgao>35</cOrgao>' +
                                          '<tpAmb>2</tpAmb>' +
                                          '<CNPJ>12345678000123</CNPJ>' +
                                          '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                                          '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                          '<tpEvento>610111</tpEvento>' +
                                          '<nSeqEvento>1</nSeqEvento>' +
                                          '<detEvento versaoEvento="4.00">' +
                                            '<evCancPrestDesacordo>' +
                                              '<descEvento>Cancelamento Prestacao do Servico em Desacordo</descEvento>' +
                                              '<nProtEvPrestDes>12345</nProtEvPrestDes>' +
                                            '</evCancPrestDesacordo>' +
                                          '</detEvento>' +
                                        '</infEvento>' +
                                      '</eventoCTe>';

  sxml_EventoGTV = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                    '<infEvento Id="ID11017012345678901234567890123456789012345678901234001">' +
                    '<cOrgao>35</cOrgao>' +
                    '<tpAmb>2</tpAmb>' +
                    '<CNPJ>12345678000123</CNPJ>' +
                    '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                    '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                    '<tpEvento>110170</tpEvento>' +
                    '<nSeqEvento>1</nSeqEvento>' +
                      '<detEvento versaoEvento="4.00">' +
                        '<evGTV>' +
                        '<descEvento>Informacoes da GTV</descEvento>' +
                          '<infGTV>' +
                            '<nDoc>123</nDoc>' +
                            '<id>12345678901234567890</id>' +
                            '<serie>1</serie>' +
                            '<subserie>0</subserie>' +
                            '<dEmi>2024-04-09</dEmi>' +
                            '<nDV>0</nDV>' +
                            '<qCarga>10.0000</qCarga>' +
                              '<infEspecie>' +
                                '<tpEspecie>1</tpEspecie>' +
                                '<vEspecie>100.00</vEspecie>' +
                              '</infEspecie>' +
                              '<rem>' +
                                '<CNPJ>12345678000123</CNPJ>' +
                                '<IE>12345678</IE>' +
                                '<UF>SP</UF>' +
                                '<xNome>Remetente</xNome>' +
                              '</rem>' +
                              '<dest>' +
                                '<CNPJ>12345678000123</CNPJ>' +
                                '<IE>12345678</IE>' +
                                '<UF>SP</UF>' +
                                '<xNome>Destinatario</xNome>' +
                              '</dest>' +
                            '<placa>ABC1234</placa>' +
                            '<UF>SP</UF>' +
                            '<RNTRC>123456</RNTRC>' +
                          '</infGTV>' +
                        '</evGTV>' +
                      '</detEvento>' +
                    '</infEvento>' +
                   '</eventoCTe>';

  sxml_EventoComprEntrega = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                              '<infEvento Id="ID11018012345678901234567890123456789012345678901234001">' +
                                '<cOrgao>35</cOrgao>' +
                                '<tpAmb>2</tpAmb>' +
                                '<CNPJ>12345678000123</CNPJ>' +
                                '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                                '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                '<tpEvento>110180</tpEvento>' +
                                '<nSeqEvento>1</nSeqEvento>' +
                                '<detEvento versaoEvento="4.00">' +
                                  '<evCECTe>' +
                                    '<descEvento>Comprovante de Entrega do CT-e</descEvento>' +
                                    '<nProt>12345</nProt>' +
                                    '<dhEntrega>2024-04-09T18:14:00-03:00</dhEntrega>' +
                                    '<nDoc>123</nDoc>' +
                                    '<xNome>Nome do Cliente</xNome>' +
                                    '<latitude>5.000000</latitude>' +
                                    '<longitude>5.000000</longitude>' +
                                    '<hashEntrega>123456</hashEntrega>' +
                                    '<dhHashEntrega>2024-04-09T18:14:00-03:00</dhHashEntrega>' +
                                    '<infEntrega>' +
                                      '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                    '</infEntrega>' +
                                  '</evCECTe>' +
                                '</detEvento>' +
                              '</infEvento>' +
                            '</eventoCTe>';

  sxml_EventoCancComprEntrega = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                                  '<infEvento Id="ID11018112345678901234567890123456789012345678901234001">' +
                                    '<cOrgao>35</cOrgao>' +
                                    '<tpAmb>2</tpAmb>' +
                                    '<CNPJ>12345678000123</CNPJ>' +
                                    '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                                    '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                    '<tpEvento>110181</tpEvento>' +
                                    '<nSeqEvento>1</nSeqEvento>' +
                                    '<detEvento versaoEvento="4.00">' +
                                      '<evCancCECTe>' +
                                        '<descEvento>Cancelamento do Comprovante de Entrega do CT-e</descEvento>' +
                                        '<nProt>123456</nProt>' +
                                        '<nProtCE>123456</nProtCE>' +
                                      '</evCancCECTe>' +
                                    '</detEvento>' +
                                  '</infEvento>' +
                                '</eventoCTe>';

  sxml_EventoInsucessoEntrega = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                                  '<infEvento Id="ID11019012345678901234567890123456789012345678901234001">' +
                                    '<cOrgao>92</cOrgao>' +
                                    '<tpAmb>2</tpAmb>' +
                                    '<CNPJ>12345678000123</CNPJ>' +
                                    '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                                    '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                    '<tpEvento>110190</tpEvento>' +
                                    '<nSeqEvento>1</nSeqEvento>' +
                                    '<detEvento versaoEvento="4.00">' +
                                      '<evIECTe>' +
                                        '<descEvento>Insucesso na Entrega do CT-e</descEvento>' +
                                        '<nProt>12345</nProt>' +
                                        '<dhTentativaEntrega>2024-04-09T18:14:00-03:00</dhTentativaEntrega>' +
                                        '<nTentativa>001</nTentativa>' +
                                        '<tpMotivo>4</tpMotivo>' +
                                        '<xJustMotivo>Destinatario mudou</xJustMotivo>' +
                                        '<latitude>5.000000</latitude>' +
                                        '<longitude>5.000000</longitude>' +
                                        '<hashTentativaEntrega>123456</hashTentativaEntrega>' +
                                        '<dhHashTentativaEntrega>2024-04-09T18:14:00-03:00</dhHashTentativaEntrega>' +
                                        '<infEntrega>' +
                                          '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                        '</infEntrega>' +
                                      '</evIECTe>' +
                                    '</detEvento>' +
                                  '</infEvento>' +
                                '</eventoCTe>';

  sxml_EventoCancInsucessoEntrega = '<eventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                                      '<infEvento Id="ID11019112345678901234567890123456789012345678901234001">' +
                                        '<cOrgao>92</cOrgao>' +
                                        '<tpAmb>2</tpAmb>' +
                                        '<CNPJ>12345678000123</CNPJ>' +
                                        '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                                        '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                        '<tpEvento>110191</tpEvento>' +
                                        '<nSeqEvento>1</nSeqEvento>' +
                                          '<detEvento versaoEvento="4.00">' +
                                          '<evCancIECTe>' +
                                            '<descEvento>Cancelamento do Insucesso de Entrega do CT-e</descEvento>' +
                                            '<nProt>123456789012345</nProt>' +
                                            '<nProtIE>123456789012345</nProtIE>' +
                                          '</evCancIECTe>' +
                                        '</detEvento>' +
                                      '</infEvento>' +
                                    '</eventoCTe>';

  { Xmls de Retorno }
  sxml_Ret_Vazio =  '';

  sxml_RetConsSit = '<retConsSitCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="4.00">' +
                      '<tpAmb>2</tpAmb>' +
                      '<verAplic>SP_PL_CTe_400</verAplic>' +
                      '<cStat>100</cStat>' +
                      '<xMotivo>Autorizado o uso do CT-e</xMotivo>' +
                      '<cUF>35</cUF>' +
                      '<protCTe versao="4.00">' +
                        '<infProt>' +
                          '<tpAmb>2</tpAmb>' +
                          '<verAplic>SP_PL_CTe_400</verAplic>' +
                          '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                          '<dhRecbto>2010-08-31T19:20:22</dhRecbto>' +
                          '<nProt>123456789012345</nProt>' +
                          '<digVal>PNG7OJ2WYLQyhkL2kWBykEGSVQA=</digVal>' +
                          '<cStat>100</cStat>' +
                          '<xMotivo>Autorizado o uso do CT-e</xMotivo>' +
                          '<infFisco>' +
                            '<cMsg>1</cMsg>' +
                            '<xMsg>Autorizado</xMsg>' +
                          '</infFisco>' +
                        '</infProt>' +
                      '</protCTe>' +
                      '<procEventoCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="1.00">' +
                        '<evento xmlns="http://www.portalfiscal.inf.br/cte" versao="1.00">' +
                          '<infEvento Id="ID1101111234567890123456789012345678901234567890123401">' +
                            '<cOrgao>35</cOrgao>' +
                            '<tpAmb>1</tpAmb>' +
                            '<CNPJ>12345678000123</CNPJ>' +
                            '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                            '<dhEvento>2018-09-18T16:07:56-03:00</dhEvento>' +
                            '<tpEvento>110111</tpEvento>' +
                            '<nSeqEvento>1</nSeqEvento>' +
                            '<verEvento>1.00</verEvento>' +
                            '<detEvento versao="1.00">' +
                              '<descEvento>Cancelamento</descEvento>' +
                              '<nProt>123456789012345</nProt>' +
                              '<xJust>erro de faturamento</xJust>' +

                              // incluir as demais tag

                            '</detEvento>' +
                          '</infEvento>' +
                        '</evento>' +
                        '<retEvento versao="1.00">' +
                          '<infEvento>' +
                            '<tpAmb>1</tpAmb>' +
                            '<verAplic>SP_PL_CTe_400</verAplic>' +
                            '<cOrgao>35</cOrgao>' +
                            '<cStat>135</cStat>' +
                            '<xMotivo>Evento registrado e vinculado ao CT-e</xMotivo>' +
                            '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                            '<tpEvento>110111</tpEvento>' +
                            '<xEvento>Cancelamento registrado</xEvento>' +
                            '<nSeqEvento>1</nSeqEvento>' +
                            '<CNPJDest>12345678000123</CNPJDest>' +
                            '<dhRegEvento>2018-09-18T16:07:56-03:00</dhRegEvento>' +
                            '<nProt>123456789012345</nProt>' +
                          '</infEvento>' +
                        '</retEvento>' +
                      '</procEventoCTe>' +
                    '</retConsSitCTe>';

  sxml_RetEnvEvento =  '<retEventoCTe versao="4.00">' +
                         '<infEvento>' +
                           '<tpAmb>2</tpAmb>' +
                           '<verAplic>SP-CTe-2023-10-05-1</verAplic>' +
                           '<cOrgao>35</cOrgao>' +
                           '<cStat>135</cStat>' +
                           '<xMotivo>Evento registrado e vinculado a CT-e.</xMotivo>' +
                           '<chCTe>12345678901234567890123456789012345678901234</chCTe>' +
                           '<tpEvento>110111</tpEvento>' +
                           '<xEvento>Cancelamento</xEvento>' +
                           '<nSeqEvento>1</nSeqEvento>' +
                           '<dhRegEvento>2023-10-23T17:15:05-03:00</dhRegEvento>' +
                           '<nProt>123456789012345</nProt>' +
                         '</infEvento>' +
                       '</retEventoCTe>';

implementation

end.
