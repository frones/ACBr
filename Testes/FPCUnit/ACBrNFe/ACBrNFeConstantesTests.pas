unit ACBrNFeConstantesTests;

{$I ACBr.inc}

interface

const
  { Xmls de Envio }
  sXml_admCSCConsulta = '<admCscNFCe xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                          '<tpAmb>2</tpAmb>' +
                          '<indOp>1</indOp>' +
                          '<raizCNPJ>12345678</raizCNPJ>' +
                        '</admCscNFCe>';

  sXml_admCSCNovo = '<admCscNFCe xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                      '<tpAmb>2</tpAmb>' +
                      '<indOp>2</indOp>' +
                      '<raizCNPJ>12345678</raizCNPJ>' +
                    '</admCscNFCe>';

  sXml_admCSCRevoga = '<admCscNFCe xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                        '<tpAmb>2</tpAmb>' +
                        '<indOp>3</indOp>' +
                        '<raizCNPJ>12345678</raizCNPJ>' +
                        '<dadosCsc>' +
                          '<idCsc>000001</idCsc>' +
                          '<codigoCsc>abc</codigoCsc>' +
                        '</dadosCsc>' +
                      '</admCscNFCe>';

  sXml_ConsSit = '<consSitNFe xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                   '<tpAmb>2</tpAmb>' +
                   '<xServ>CONSULTAR</xServ>' +
                   '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                 '</consSitNFe>';

  sXml_Inut = '<inutNFe xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                '<infInut Id="ID35241234567800012355001000000010000000020">' +
                  '<tpAmb>2</tpAmb>' +
                  '<xServ>INUTILIZAR</xServ>' +
                  '<cUF>35</cUF>' +
                  '<ano>24</ano>' +
                  '<CNPJ>12345678000123</CNPJ>' +
                  '<mod>55</mod>' +
                  '<serie>1</serie>' +
                  '<nNFIni>10</nNFIni>' +
                  '<nNFFin>20</nNFFin>' +
                  '<xJust>Erro no Sistema de Emissao de Notas</xJust>' +
                '</infInut>' +
              '</inutNFe>';

  sxml_EventoCCe = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                     '<idLote>1</idLote>' +
                     '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                       '<infEvento Id="ID1101101234567890123456789012345678901234567890123401">' +
                         '<cOrgao>35</cOrgao>' +
                         '<tpAmb>2</tpAmb>' +
                         '<CNPJ>12345678000123</CNPJ>' +
                         '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                         '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                         '<tpEvento>110110</tpEvento>' +
                         '<nSeqEvento>1</nSeqEvento>' +
                         '<verEvento>4.00</verEvento>' +
                         '<detEvento versao="4.00">' +
                           '<descEvento>Carta de Correcao</descEvento>' +
                           '<xCorrecao>Descricao do produto errada</xCorrecao>' +
                           '<xCondUso>A Carta de Correcao e disciplinada pelo paragrafo 1o-A do ' +
                           'art. 7o do Convenio S/N, de 15 de dezembro de 1970 e pode ser utilizada ' +
                           'para regularizacao de erro ocorrido na emissao de documento fiscal, ' +
                           'desde que o erro nao esteja relacionado com: I - as variaveis que ' +
                           'determinam o valor do imposto tais como: base de calculo, aliquota, ' +
                           'diferenca de preco, quantidade, valor da operacao ou da prestacao; ' +
                           'II - a correcao de dados cadastrais que implique mudanca do remetente ' +
                           'ou do destinatario; III - a data de emissao ou de saida.</xCondUso>' +
                         '</detEvento>' +
                       '</infEvento>' +
                     '</evento>' +
                   '</envEvento>';

  sxml_EventoCancelamento = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                              '<idLote>1</idLote>' +
                              '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                '<infEvento Id="ID1101111234567890123456789012345678901234567890123401">' +
                                  '<cOrgao>35</cOrgao>' +
                                  '<tpAmb>2</tpAmb>' +
                                  '<CNPJ>12345678000123</CNPJ>' +
                                  '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                  '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                  '<tpEvento>110111</tpEvento>' +
                                  '<nSeqEvento>1</nSeqEvento>' +
                                  '<verEvento>4.00</verEvento>' +
                                  '<detEvento versao="4.00">' +
                                    '<descEvento>Cancelamento</descEvento>' +
                                    '<nProt>123456</nProt>' +
                                    '<xJust>Dados Errados Informados na Nota</xJust>' +
                                  '</detEvento>' +
                                '</infEvento>' +
                              '</evento>' +
                            '</envEvento>';

  sxml_EventoCancSubst = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                           '<idLote>1</idLote>' +
                           '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                             '<infEvento Id="ID1101121234567890123456789012345678901234567890123401">' +
                               '<cOrgao>35</cOrgao>' +
                               '<tpAmb>2</tpAmb>' +
                               '<CNPJ>12345678000123</CNPJ>' +
                               '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                               '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                               '<tpEvento>110112</tpEvento>' +
                               '<nSeqEvento>1</nSeqEvento>' +
                               '<verEvento>4.00</verEvento>' +
                               '<detEvento versao="4.00">' +
                                 '<descEvento>Cancelamento por substituicao</descEvento>' +
                                 '<cOrgaoAutor>35</cOrgaoAutor>' +
                                 '<tpAutor>1</tpAutor>' +
                                 '<verAplic>1.00</verAplic>' +
                                 '<nProt>123456</nProt>' +
                                 '<xJust>Dados Errados Informados na Nota</xJust>' +
                                 '<chNFeRef>12345678901234567890123456789012345678901234</chNFeRef>' +
                               '</detEvento>' +
                             '</infEvento>' +
                           '</evento>' +
                         '</envEvento>';

  sxml_EventoManifDestConf = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                               '<idLote>1</idLote>' +
                               '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                 '<infEvento Id="ID2102001234567890123456789012345678901234567890123401">' +
                                   '<cOrgao>35</cOrgao>' +
                                   '<tpAmb>2</tpAmb>' +
                                   '<CNPJ>12345678000123</CNPJ>' +
                                   '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                   '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                   '<tpEvento>210200</tpEvento>' +
                                   '<nSeqEvento>1</nSeqEvento>' +
                                   '<verEvento>4.00</verEvento>' +
                                   '<detEvento versao="4.00">' +
                                     '<descEvento>Confirmacao da Operacao</descEvento>' +
                                   '</detEvento>' +
                                 '</infEvento>' +
                               '</evento>' +
                             '</envEvento>';

  sxml_EventoManifDestCiencia = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                  '<idLote>1</idLote>' +
                                  '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                    '<infEvento Id="ID2102101234567890123456789012345678901234567890123401">' +
                                      '<cOrgao>35</cOrgao>' +
                                      '<tpAmb>2</tpAmb>' +
                                      '<CNPJ>12345678000123</CNPJ>' +
                                      '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                      '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                      '<tpEvento>210210</tpEvento>' +
                                      '<nSeqEvento>1</nSeqEvento>' +
                                      '<verEvento>4.00</verEvento>' +
                                      '<detEvento versao="4.00">' +
                                        '<descEvento>Ciencia da Operacao</descEvento>' +
                                      '</detEvento>' +
                                    '</infEvento>' +
                                  '</evento>' +
                                '</envEvento>';

  sxml_EventoManifDesconhecimento = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                      '<idLote>1</idLote>' +
                                      '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                        '<infEvento Id="ID2102201234567890123456789012345678901234567890123401">' +
                                          '<cOrgao>35</cOrgao>' +
                                          '<tpAmb>2</tpAmb>' +
                                          '<CNPJ>12345678000123</CNPJ>' +
                                          '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                          '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                          '<tpEvento>210220</tpEvento>' +
                                          '<nSeqEvento>1</nSeqEvento>' +
                                          '<verEvento>4.00</verEvento>' +
                                          '<detEvento versao="4.00">' +
                                            '<descEvento>Desconhecimento da Operacao</descEvento>' +
                                          '</detEvento>' +
                                        '</infEvento>' +
                                      '</evento>' +
                                    '</envEvento>';

  sxml_EventoManiNaoRealizada = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                  '<idLote>1</idLote>' +
                                  '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                    '<infEvento Id="ID2102401234567890123456789012345678901234567890123401">' +
                                      '<cOrgao>35</cOrgao>' +
                                      '<tpAmb>2</tpAmb>' +
                                      '<CNPJ>12345678000123</CNPJ>' +
                                      '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                      '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                      '<tpEvento>210240</tpEvento>' +
                                      '<nSeqEvento>1</nSeqEvento>' +
                                      '<verEvento>4.00</verEvento>' +
                                      '<detEvento versao="4.00">' +
                                        '<descEvento>Operacao nao Realizada</descEvento>' +
                                        '<xJust>Produto diferente do pedido</xJust>' +
                                      '</detEvento>' +
                                    '</infEvento>' +
                                  '</evento>' +
                                '</envEvento>';

  sxml_EventoEPEC = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                      '<idLote>1</idLote>' +
                      '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                        '<infEvento Id="ID1101401234567890123456789012345678901234567890123401">' +
                          '<cOrgao>35</cOrgao>' +
                          '<tpAmb>2</tpAmb>' +
                          '<CNPJ>12345678000123</CNPJ>' +
                          '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                          '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                          '<tpEvento>110140</tpEvento>' +
                          '<nSeqEvento>1</nSeqEvento>' +
                          '<verEvento>4.00</verEvento>' +
                          '<detEvento versao="4.00">' +
                            '<descEvento>EPEC</descEvento>' +
                            '<cOrgaoAutor>35</cOrgaoAutor>' +
                            '<tpAutor>1</tpAutor>' +
                            '<verAplic>1.00</verAplic>' +
                            '<dhEmi>2024-04-09T18:14:00-03:00</dhEmi>' +
                            '<tpNF>1</tpNF>' +
                            '<IE>12345</IE>' +
                            '<dest>' +
                              '<UF>SP</UF>' +
                              '<CNPJ>12345678000123</CNPJ>' +
                            '</dest>' +
                            '<vNF>10.00</vNF>' +
                            '<vICMS>10.00</vICMS>' +
                          '</detEvento>' +
                        '</infEvento>' +
                      '</evento>' +
                    '</envEvento>';

  sxml_EventoPedProrrog = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                            '<idLote>1</idLote>' +
                            '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                              '<infEvento Id="ID1115001234567890123456789012345678901234567890123401">' +
                                '<cOrgao>35</cOrgao>' +
                                '<tpAmb>2</tpAmb>' +
                                '<CNPJ>12345678000123</CNPJ>' +
                                '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                '<tpEvento>111500</tpEvento>' +
                                '<nSeqEvento>1</nSeqEvento>' +
                                '<verEvento>4.00</verEvento>' +
                                '<detEvento versao="4.00">' +
                                  '<descEvento>Pedido de Prorrogacao</descEvento>' +
                                  '<nProt>123456</nProt>' +
                                  '<itemPedido numItem="1">' +
                                    '<qtdeItem>10.00</qtdeItem>' +
                                  '</itemPedido>' +
                                '</detEvento>' +
                              '</infEvento>' +
                            '</evento>' +
                          '</envEvento>';

  sxml_EventoCanPedProrrog1 = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                '<idLote>1</idLote>' +
                                '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                  '<infEvento Id="ID1115021234567890123456789012345678901234567890123401">' +
                                  '<cOrgao>35</cOrgao>' +
                                  '<tpAmb>2</tpAmb>' +
                                  '<CNPJ>12345678000123</CNPJ>' +
                                  '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                  '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                  '<tpEvento>111502</tpEvento>' +
                                  '<nSeqEvento>1</nSeqEvento>' +
                                  '<verEvento>4.00</verEvento>' +
                                  '<detEvento versao="4.00">' +
                                    '<descEvento>Cancelamento de Pedido de Prorrogacao</descEvento>' +
                                    '<idPedidoCancelado>123456</idPedidoCancelado>' +
                                    '<nProt>123456</nProt>' +
                                  '</detEvento>' +
                                  '</infEvento>' +
                                '</evento>' +
                              '</envEvento>';

  sxml_EventoComprEntrega = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                              '<idLote>1</idLote>' +
                              '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                '<infEvento Id="ID1101301234567890123456789012345678901234567890123401">' +
                                  '<cOrgao>35</cOrgao>' +
                                  '<tpAmb>2</tpAmb>' +
                                  '<CNPJ>12345678000123</CNPJ>' +
                                  '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                  '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                  '<tpEvento>110130</tpEvento>' +
                                  '<nSeqEvento>1</nSeqEvento>' +
                                  '<verEvento>4.00</verEvento>' +
                                  '<detEvento versao="4.00">' +
                                    '<descEvento>Comprovante de Entrega da NF-e</descEvento>' +
                                    '<cOrgaoAutor>35</cOrgaoAutor>' +
                                    '<tpAutor>1</tpAutor>' +
                                    '<verAplic>1.00</verAplic>' +
                                    '<dhEntrega>2024-04-09T18:14:00-03:00</dhEntrega>' +
                                    '<nDoc>123</nDoc>' +
                                    '<xNome>Nome do Cliente</xNome>' +
                                    '<latGPS>10.000000</latGPS>' +
                                    '<longGPS>20.000000</longGPS>' +
                                    '<hashComprovante>123456</hashComprovante>' +
                                    '<dhHashComprovante>2024-04-09T18:14:00-03:00</dhHashComprovante>' +
                                  '</detEvento>' +
                                '</infEvento>' +
                              '</evento>' +
                            '</envEvento>';

  sxml_EventoCancComprEntrega = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                  '<idLote>1</idLote>' +
                                  '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                    '<infEvento Id="ID1101311234567890123456789012345678901234567890123401">' +
                                      '<cOrgao>35</cOrgao>' +
                                      '<tpAmb>2</tpAmb>' +
                                      '<CNPJ>12345678000123</CNPJ>' +
                                      '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                      '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                      '<tpEvento>110131</tpEvento>' +
                                      '<nSeqEvento>1</nSeqEvento>' +
                                      '<verEvento>4.00</verEvento>' +
                                      '<detEvento versao="4.00">' +
                                        '<descEvento>Cancelamento Comprovante de Entrega da NF-e</descEvento>' +
                                        '<cOrgaoAutor>35</cOrgaoAutor>' +
                                        '<tpAutor>1</tpAutor>' +
                                        '<verAplic>1.00</verAplic>' +
                                        '<nProtEvento>123456</nProtEvento>' +
                                      '</detEvento>' +
                                    '</infEvento>' +
                                  '</evento>' +
                                '</envEvento>';

  sxml_EventoAtorInteressado = '<envEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                 '<idLote>1</idLote>' +
                                 '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                                   '<infEvento Id="ID1101501234567890123456789012345678901234567890123401">' +
                                     '<cOrgao>35</cOrgao>' +
                                     '<tpAmb>2</tpAmb>' +
                                     '<CNPJ>12345678000123</CNPJ>' +
                                     '<chNFe>12345678901234567890123456789012345678901234</chNFe>' +
                                     '<dhEvento>2024-04-09T18:14:00-03:00</dhEvento>' +
                                     '<tpEvento>110150</tpEvento>' +
                                     '<nSeqEvento>1</nSeqEvento>' +
                                     '<verEvento>4.00</verEvento>' +
                                     '<detEvento versao="4.00">' +
                                       '<descEvento>Ator interessado na NF-e</descEvento>' +
                                       '<cOrgaoAutor>35</cOrgaoAutor>' +
                                       '<tpAutor>1</tpAutor>' +
                                       '<verAplic>1.00</verAplic>' +
                                       '<autXML>' +
                                         '<CNPJ>12345678000123</CNPJ>' +
                                       '</autXML>' +
                                       '<tpAutorizacao>1</tpAutorizacao>' +
                                       '<xCondUso>O emitente ou destinatário da NF-e, declara que permite ' +
                                       'o transportador declarado no campo CNPJ/CPF deste evento a ' +
                                       'autorizar os transportadores subcontratados ou redespachados a ' +
                                       'terem acesso ao download da NF-e</xCondUso>' +
                                     '</detEvento>' +
                                   '</infEvento>' +
                                 '</evento>' +
                               '</envEvento>';

  { Xmls de Retorno }
  sxml_Ret_Vazio =  '';

  sxml_RetAdmCSC = '<retAdmCscNFCe xmlns="http://www.portalfiscal.inf.br/nfe" versao="1.00">' +
                     '<tpAmb>2</tpAmb>' +
                     '<indOp>1</indOp>' +
                     '<cStat>150</cStat>' +
                     '<xMotivo>Consulta de CSC realizada</xMotivo>' +
                     '<dadosCsc>' +
                       '<idCsc>000001</idCsc>' +
                       '<codigoCsc>abc</codigoCsc>' +
                     '</dadosCsc>' +
                   '</retAdmCscNFCe>';

  sxml_RetConsSit = '<retConsSitNFe xmlns="http://www.portalfiscal.inf.br/nfe" versao="4.00">' +
                      '<tpAmb>2</tpAmb>' +
                      '<verAplic>SP_NFE_PL_006e</verAplic>' +
                      '<cStat>100</cStat>' +
                      '<xMotivo>Autorizado o uso da NF-e</xMotivo>' +
                      '<cUF>35</cUF>' +
                      '<chNFe>35100804550110000188550010000000101204117493</chNFe>' +
                      '<protNFe versao="4.00">' +
                        '<infProt>' +
                          '<tpAmb>2</tpAmb>' +
                          '<verAplic>SP_NFE_PL_006e</verAplic>' +
                          '<chNFe>35100804550110000188550010000000101204117493</chNFe>' +
                          '<dhRecbto>2010-08-31T19:20:22</dhRecbto>' +
                          '<nProt>135100025493261</nProt>' +
                          '<digVal>PNG7OJ2WYLQyhkL2kWBykEGSVQA=</digVal>' +
                          '<cStat>100</cStat>' +
                          '<xMotivo>Autorizado o uso da NF-e</xMotivo>' +
                          '<cMsg>1</cMsg>' +
                          '<xMsg>Autorizado</xMsg>' +
                        '</infProt>' +
                      '</protNFe>' +
                      '<procEventoNFe xmlns="http://www.portalfiscal.inf.br/nfe" versao="1.00">' +
                        '<evento xmlns="http://www.portalfiscal.inf.br/nfe" versao="1.00">' +
                          '<infEvento Id="ID1101113518090569453700011255001000144972100144972501">' +
                            '<cOrgao>35</cOrgao>' +
                            '<tpAmb>1</tpAmb>' +
                            '<CNPJ>05694537000112</CNPJ>' +
                            '<chNFe>35180905694537000112550010001449721001449725</chNFe>' +
                            '<dhEvento>2018-09-18T16:07:56-03:00</dhEvento>' +
                            '<tpEvento>110111</tpEvento>' +
                            '<nSeqEvento>1</nSeqEvento>' +
                            '<verEvento>1.00</verEvento>' +
                            '<detEvento versao="1.00">' +
                              '<descEvento>Cancelamento</descEvento>' +
                              '<nProt>135180636988450</nProt>' +
                              '<xJust>erro de faturamento</xJust>' +

                              // incluir as demais tag

                            '</detEvento>' +
                          '</infEvento>' +
                        '</evento>' +
                        '<retEvento versao="1.00">' +
                          '<infEvento>' +
                            '<tpAmb>1</tpAmb>' +
                            '<verAplic>SP_EVENTOS_PL_100</verAplic>' +
                            '<cOrgao>35</cOrgao>' +
                            '<cStat>135</cStat>' +
                            '<xMotivo>Evento registrado e vinculado a NF-e</xMotivo>' +
                            '<chNFe>35180905694537000112550010001449721001449725</chNFe>' +
                            '<tpEvento>110111</tpEvento>' +
                            '<xEvento>Cancelamento registrado</xEvento>' +
                            '<nSeqEvento>1</nSeqEvento>' +
                            '<CNPJDest>04550110000188</CNPJDest>' +
                            '<dhRegEvento>2018-09-18T16:07:56-03:00</dhRegEvento>' +
                            '<nProt>135180637085421</nProt>' +
                          '</infEvento>' +
                        '</retEvento>' +
                      '</procEventoNFe>' +
                    '</retConsSitNFe>';

  sxml_RetInut =  '<retInutNFe xmlns="http://www.portalfiscal.inf.br/nfe" versao="3.00">' +
                    '<infInut Id="ID113130001934975">' +
                      '<tpAmb>2</tpAmb>' +
                      '<verAplic>AM3.00</verAplic>' +
                      '<cStat>102</cStat>' +
                      '<xMotivo>Inutilizacao de numero homologado</xMotivo>' +
                      '<cUF>13</cUF>' +
                      '<dhRecbto>2013-01-24T19:46:32-04:00</dhRecbto>' +
                      '<ano>13</ano>' +
                      '<CNPJ>12345678000123</CNPJ>' +
                      '<mod>65</mod>' +
                      '<serie>1</serie>' +
                      '<nNFIni>16</nNFIni>' +
                      '<nNFFin>18</nNFFin>' +
                      '<nProt>113130001934975</nProt>' +
                    '</infInut>' +
                  '</retInutNFe>';

  sxml_RetEnvEvento =  '<retEnvEvento xmlns="http://www.portalfiscal.inf.br/nfe" versao="1.00">' +
                         '<idLote>8</idLote>' +
                         '<tpAmb>1</tpAmb>' +
                         '<verAplic>AN_1.0.0</verAplic>' +
                         '<cOrgao>91</cOrgao>' +
                         '<cStat>128</cStat>' +
                         '<xMotivo>Lote de evento processado</xMotivo>' +
                         '<retEvento versao="1.00">' +
                           '<infEvento Id="ID891120000074187">' +
                             '<tpAmb>1</tpAmb>' +
                             '<verAplic>AN_1.0.0</verAplic>' +
                             '<cOrgao>91</cOrgao>' +
                             '<cStat>135</cStat>' +
                             '<xMotivo>Evento registrado e vinculado a NF-e</xMotivo>' +
                             '<chNFe>35121012345678000123550010000003911000003915</chNFe>' +
                             '<tpEvento>210210</tpEvento>' +
                             '<nSeqEvento>1</nSeqEvento>' +
                             '<dhRegEvento>2012-11-21T23:51:55-02:00</dhRegEvento>' +
                             '<nProt>891120000074187</nProt>' +
                           '</infEvento>' +
                         '</retEvento>' +
                         '<retEvento versao="1.00">' +
                           '<infEvento Id="ID891120000074188">' +
                             '<tpAmb>1</tpAmb>' +
                             '<verAplic>AN_1.0.0</verAplic>' +
                             '<cOrgao>91</cOrgao>' +
                             '<cStat>135</cStat>' +
                             '<xMotivo>Evento registrado e vinculado a NF-e</xMotivo>' +
                             '<chNFe>35121012345678000123550010000068961010068962</chNFe>' +
                             '<tpEvento>210210</tpEvento>' +
                             '<nSeqEvento>1</nSeqEvento>' +
                             '<dhRegEvento>2012-11-21T23:51:55-02:00</dhRegEvento>' +
                             '<nProt>891120000074188</nProt>' +
                           '</infEvento>' +
                         '</retEvento>' +
                       '</retEnvEvento>';

implementation

end.
