unit ACBrCTeXmlReaderReadingTests_CTeOS_Ver400;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ACBrCTeXmlHandler, ACBrTests.Util, pcteCTe;

type

  { TACBrCTeXmlReaderReadingTests_CTeOS_Ver400 }

  TACBrCTeXmlReaderReadingTests_CTeOS_Ver400 = class(TTestCase)
  private
    FCTe: TCTe;
    FCTeXmlReader: TCTeXmlReader;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LerXML_LeuCorretamenteOsAtributosDoGrupo_InfCTe;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ide;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_compl;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_emit;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_toma;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_vPrest;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_imp;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infServico;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infDocRef;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_seg;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_rodoOS;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_cobr;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infGTVe;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_autXML;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infRespTec;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infCTeSupl;
  end;

implementation

uses
  ACBrCTeTestConsts, ACBrUtil.DateTime, pcnConversao, pcteConversaoCTe;

{ TACBrCTeXmlReaderReadingTests_CTeOS_Ver400 }

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.SetUp;
begin
  inherited SetUp;
  FCTe := TCTe.Create;
  FCTeXmlReader := TCTeXmlReader.Create(FCTe);
  FCTeXmlReader.CarregarArquivo(XML_CTEOS_NORMAL_VERSAO400);
  FCTeXmlReader.LerXML;
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.TearDown;
begin
  FCTeXmlReader.Free;
  FCTe.Free;
  inherited TearDown;
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsAtributosDoGrupo_InfCTe;
begin
  AssertEquals('CTe.InfCTe.Versao', 4, FCTe.infCTe.versao);
  AssertEquals('CTe.infCTe.Id', 'CTe35240118760500000000670010000000011434778690', FCTe.infCTe.Id);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ide;
begin
  AssertEquals('CTe.ide.cUF', 35, FCTe.ide.cUF);
  AssertEquals('CTe.ide.cCT', 43477869, FCTe.ide.cCT);
  AssertEquals('CTe.ide.CFOP', 6932, FCTe.ide.CFOP);
  AssertEquals('CTe.ide.natOP', 'PRESTACAO SERVICO TRANSPORTE INICIO OUTRA UF FORA DO ESTADO', FCTe.ide.natOp);
  AssertEquals('CTe.ide.mod', 67, FCTe.ide.modelo);
  AssertEquals('CTe.ide.serie', 1, FCTe.ide.serie);
  AssertEquals('CTe.ide.nCT', 1, FCTe.ide.nCT);
  AssertEquals('CTe.ide.dhEmi', EncodeDataHora('26/01/2024 10:12:11', 'dd/mm/yyyy hh:nn:ss'), FCTe.ide.dhEmi);
  AssertEquals('CTe.ide.tpImp', '1', TpImpToStr(FCTe.ide.tpImp));
  AssertEquals('Cte.ide.tpEmis', '1', TpEmisToStr(FCTe.ide.tpEmis));
  AssertEquals('CTe.ide.cDV', 0, FCTe.ide.cDV);
  AssertEquals('CTe.ide.tpAmb', '2', TpAmbToStr(FCTe.ide.tpAmb));
  AssertEquals('CTe.ide.tpCTe', '0', tpCTePagToStr(FCTe.ide.tpCte));
  AssertEquals('CTe.ide.procEmi', '0', procEmiToStr(FCTe.ide.procEmi));
  AssertEquals('CTe.ide.verProc', '3.0', FCTe.ide.verProc);
  AssertEquals('CTe.ide.indGlobalizado', '0', TIndicadorToStr(FCTe.ide.indGlobalizado));//0-Não 1-Sim
  AssertEquals('CTe.ide.cMunEnv', 3554003, FCTe.ide.cMunEnv);
  AssertEquals('CTe.ide.xMunEnv', 'TATUI', FCTe.ide.xMunEnv);
  AssertEquals('CTe.ide.UFEnv', 'SP', FCTe.ide.UFEnv);
  AssertEquals('CTe.ide.modal', '01', TpModalToStr(FCTe.ide.modal));
  AssertEquals('CTe.ide.tpServ', '6', TpServPagToStr(FCTe.ide.tpServ));
  AssertEquals('CTe.ide.cMunIni', 3119401, FCTe.ide.cMunIni);
  AssertEquals('CTe.ide.xMunIni', 'CORONEL FABRICIANO', FCTe.ide.xMunIni);
  AssertEquals('CTe.ide.UFIni', 'MG', FCTe.ide.UFIni);
  AssertEquals('CTe.ide.cMunFim', 2900207, FCTe.ide.cMunFim);
  AssertEquals('CTe.ide.xMunFim', 'ABARE', FCTe.ide.xMunFim);
  AssertEquals('CTe.ide.UFFim', 'BA', FCTe.ide.UFFim);
  AssertEquals('CTe.ide.IndIeToma', '1', indIEDestToStr(FCTe.ide.indIEToma));
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_compl;
begin
  AssertEquals('CTe.compl.xCaracAd', 'xCaracAd', FCTe.compl.xCaracAd);
  AssertEquals('CTe.compl.xCaracSer', 'xCaracSer', FCTe.compl.xCaracSer);
  AssertEquals('CTe.compl.xEmi', 'xEmi', FCTe.compl.xEmi);
  AssertEquals('CTe.compl.xObs', 'xObs', FCTe.compl.xObs);

  //ObsCont
  AssertEquals('CTe.compl.ObsCont.Count', 1, FCTe.compl.ObsCont.Count);
  AssertEquals('CTe.compl.obsCont[0].xCampo', 'xCampo1', FCTe.compl.ObsCont[0].xCampo);
  AssertEquals('CTe.compl.ObsCont[0].xTexto', 'xTexto1', FCTe.compl.obsCont[0].xTexto);

  //ObsFisco
  AssertEquals('CTe.compl.ObsFisco.Count', 1, FCTe.compl.obsFisco.Count);
  AssertEquals('CTe.compl.obsFisco[0].xCampo', 'xCampo1', FCTe.compl.ObsFisco[0].xCampo);
  AssertEquals('CTe.compl.obsFisco[0].xTexto', 'xTexto1', FCTe.compl.ObsFisco[0].xTexto);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_emit;
begin
  AssertEquals('CTe.emit.CNPJ', '18760500000000', FCTe.emit.CNPJ);
  AssertEquals('CTe.emit.IE', '687090000000', FCTe.emit.IE);
  AssertEquals('CTe.emit.IEST', EmptyStr, FCTe.emit.IEST);
  AssertEquals('CTe.emit.xNome', 'RAZAO SOCIAL DE TESTE', FCTe.emit.xNome);
  AssertEquals('CTe.emit.xFant', 'FANTASIA DE TESTE', FCTe.emit.xFant);

  //enderEmit
  AssertEquals('CTe.emit.enderEmit.xLgr', 'Logradouro', FCTe.emit.enderEmit.xLgr);
  AssertEquals('CTe.emit.enderEmit.nro', '1', FCTe.emit.enderEmit.nro);
  AssertEquals('CTe.emit.enderEmit.xCpl', 'Complemento', FCTe.emit.enderEmit.xCpl);
  AssertEquals('CTe.emit.enderEmit.xBairro', 'Bairro', FCTe.emit.enderEmit.xBairro);
  AssertEquals('CTe.emit.enderEmit.cMun', 3554003, FCTe.emit.enderEmit.cMun);
  AssertEquals('CTe.emit.enderEmit.xMun', 'TATUI', FCTe.emit.enderEmit.xMun);
  AssertEquals('CTe.emit.enderEmit.CEP', 17250000, FCTe.emit.enderEmit.CEP);
  AssertEquals('CTe.emit.enderEmit.UF', 'SP', FCTe.emit.enderEmit.UF);
  AssertEquals('CTe.emit.enderEmit.fone', '22223333', FCTe.emit.enderEmit.fone);

  //CRT
  AssertEquals('CTe.emit.CRT', '3', CRTCTeToStr(FCTe.emit.CRT));
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_toma;
begin
  AssertEquals('CTe.toma.CNPJCPF', '10242141000174', FCTe.toma.CNPJCPF);
  AssertEquals('CTe.toma.IE', '0010834420031', FCTe.toma.IE);
  AssertEquals('CTe.toma.xNome', 'ACOUGUE E SUPERMERCADO SOUZA LTDA', FCTe.toma.xNome);
  AssertEquals('CTe.toma.xFant', 'ACOUGUE E SUPERMERCADO SOUZA', FCTe.toma.xFant);
  AssertEquals('CTe.toma.fone', '55557777', FCTe.toma.fone);
  AssertEquals('CTe.toma.email', 'toma@mail.com', FCTe.toma.email);

  //enderToma
  AssertEquals('CTe.toma.enderToma.xLgr', 'RUA BELO HORIZONTE', FCTe.toma.enderToma.xLgr);
  AssertEquals('CTe.toma.enderToma.nro', '614', FCTe.toma.enderToma.nro);
  AssertEquals('CTe.toma.enderToma.xCpl', 'N D', FCTe.toma.enderToma.xCpl);
  AssertEquals('CTe.toma.enderToma.xBairro', 'CALADINA', FCTe.toma.enderToma.xBairro);
  AssertEquals('CTe.toma.enderToma.cMun', 3119401, FCTe.toma.enderToma.cMun);
  AssertEquals('CTe.toma.enderToma.xMun', 'CORONEL FABRICIANO', FCTe.toma.enderToma.xMun);
  AssertEquals('CTe.toma.enderToma.CEP', 35171167, FCTe.toma.enderToma.CEP);
  AssertEquals('CTe.toma.enderToma.UF', 'MG', FCTe.toma.enderToma.UF);
  AssertEquals('CTe.toma.enderToma.cPais', 1058, FCTe.toma.enderToma.cPais);
  AssertEquals('CTe.toma.enderToma.xPais', 'BRASIL', FCTe.toma.enderToma.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_vPrest;
begin
  AssertEquals('CTe.vPrest.vTPrest', 100.00, FCTe.vPrest.vTPrest);
  AssertEquals('CTe.vPrest.vRec', 100.00, FCTe.vPrest.vRec);

  //comp
  AssertEquals('CTe.vPrest.comp.Count', 2, FCTe.vPrest.Comp.Count);
  AssertEquals('CTe.vPrest.comp[0].xNome', 'Componente 1', FCTe.vPrest.Comp[0].xNome);
  AssertEquals('CTe.vPrest.comp[0].vComp', 30.00, FCTe.vPrest.Comp[0].vComp);
  AssertEquals('CTe.vPrest.comp[1].xNome', 'Componente 2', FCTe.vPrest.Comp[1].xNome);
  AssertEquals('CTe.vPrest.comp[1].vComp', 70.00, FCTe.vPrest.Comp[1].vComp);

end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_imp;
begin
  //ICMSOutrasUF
  AssertEquals('CTe.imp.ICMS.ICMSOutraUF.CST', '90', CSTICMSToStr(FCTe.imp.ICMS.ICMSOutraUF.CST));
  AssertEquals('CTe.imp.ICMS.ICMSOutraUF.vBCOutraUF', 100.00, FCTe.imp.ICMS.ICMSOutraUF.vBCOutraUF);
  AssertEquals('CTe.imp.ICMS.ICMSOutraUF.pICMSOutraUF', 7.00, FCTe.imp.ICMS.ICMSOutraUF.pICMSOutraUF);
  AssertEquals('CTe.imp.ICMS.ICMSOutraUF.vICMSOutraUF', 7.00, FCTe.imp.ICMS.ICMSOutraUF.vICMSOutraUF);

  AssertEquals('CTe.imp.vTotTrib', 17.00, FCTe.imp.vTotTrib);
  AssertEquals('CTe.imp.infAdFisco', 'Lei da Transparencia: O valor aproximado de tributos incidentes sobre o preco deste servico e de R$ 17,00 (17,00%) Fonte: IBPT', FCTe.imp.infAdFisco);

  //ICMSUFFim
  AssertEquals('CTe.imp.ICMSUFFim.vBCUFFim', 100.00, FCTe.imp.ICMSUFFim.vBCUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.pFCPUFFim', 10.00, FCTe.imp.ICMSUFFim.pFCPUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.pICMSUFFim', 10.00, FCTe.imp.ICMSUFFim.pICMSUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.pICMSInter', 5.00, FCTe.imp.ICMSUFFim.pICMSInter);
  AssertEquals('CTe.imp.ICMSUFFim.vFCPUFFim', 10.00, FCTe.imp.ICMSUFFim.vFCPUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.vICMSUFFim', 10.00, FCTe.imp.ICMSUFFim.vICMSUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.vICMSUFIni', 8.00, FCTe.imp.ICMSUFFim.vICMSUFIni);

  //infTribFed
  AssertEquals('CTe.imp.infTribFed.vPIS', 1.00, FCTe.imp.infTribFed.vPIS);
  AssertEquals('CTe.imp.infTribFed.vCOFINS', 2.00, FCTe.imp.infTribFed.vCOFINS);
  AssertEquals('CTe.imp.infTribFed.vIR', 3.00, FCTe.imp.infTribFed.vIR);
  AssertEquals('CTe.imp.infTribFed.vINSS', 4.00, FCTe.imp.infTribFed.vINSS);
  AssertEquals('CTe.imp.infTribFed.vCSLL', 5.00, FCTe.imp.infTribFed.vCSLL);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infServico;
begin
  AssertEquals('CTe.infCTeNorm.infServico.xDescServ', 'TEJEJRBEFR ERFERF TESTET JFREJ', FCTe.infCTeNorm.infServico.xDescServ);
  AssertEquals('CTe.infCTeNorm.infServico.qCarga', 5000.0000, FCTe.infCTeNorm.infServico.qCarga);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infDocRef;
begin
  AssertEquals('CTe.infCTeNorm.infDocRef.Count', 2, FCTe.infCTeNorm.infDocRef.Count);
  AssertEquals('CTe.infCTeNorm.infDocRef[0].nDoc', '123', FCTe.infCTeNorm.infDocRef[0].nDoc);
  AssertEquals('CTe.infCTeNorm.infDocRef[0].serie', '1', FCTe.infCTeNorm.infDocRef[0].serie);
  AssertEquals('CTe.infCTeNorm.infDocRef[0].subserie', 'A', FCTe.infCTeNorm.infDocRef[0].subserie);
  AssertEquals('CTe.infCTeNorm.infDocRef[0].dEmi', EncodeDataHora('26/01/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.infDocRef[0].dEmi);
  AssertEquals('CTe.infCTeNorm.infDocRef[0].vDoc', 100.00, FCTe.infCTeNorm.infDocRef[0].vDoc);

  AssertEquals('CTe.infCTeNorm.infDocRef[1].chBPe', '35240118760500000000630010000000011606194254', FCTe.infCTeNorm.infDocRef[1].chBPe);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_seg;
begin
  AssertEquals('CTe.infCteNorm.seg.Count', 1, FCTe.infCteNorm.seg.Count);
  AssertEquals('CTe.infCTeNorm.seg[0].respSeg', '5', TpRspSeguroToStr(FCTe.infCteNorm.seg[0].respSeg));
  AssertEquals('CTe.infCTeNorm.seg[0].xSeg', 'TESTE', FCTe.infCteNorm.seg[0].xSeg);
  AssertEquals('CTe.infCTeNorm.seg[0].nApol', '3743784738473847', FCTe.infCteNorm.seg[0].nApol);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_rodoOS;
begin
  AssertEquals('CTe.infCteNorm.rodoOS.TAF', '454545445454', FCTe.infCTeNorm.rodoOS.TAF);
  AssertEquals('CTe.infCteNorm.rodoOS.NroRegEstadual', EmptyStr, FCTe.infCTeNorm.rodoOS.NroRegEstadual);
  AssertEquals('CTe.infCteNorm.rodoOS.veic.placa', 'MBC2448', FCTe.infCteNorm.rodoOS.veic.placa);
  AssertEquals('CTe.infCteNorm.rodoOS.veic.RENAVAM', '00709229895', FCTe.infCteNorm.rodoOS.veic.RENAVAM);
  AssertEquals('CTe.infCteNorm.rodoOS.veic.UF', 'SC', FCTe.infCTeNorm.rodoOS.veic.UF);
  AssertEquals('CTe.infCteNorm.rodoOS.prop.CNPJCPF', '44444444444', FCTe.infCTeNorm.rodoOS.veic.prop.CNPJCPF);
  AssertEquals('CTe.infCteNorm.rodoOS.prop.TAF', '454545445454', FCTe.infCteNorm.rodoOs.veic.prop.TAF);
  AssertEquals('CTe.infCteNorm.rodoOS.prop.NroRegEstadual', EmptyStr, FCTe.infCteNorm.rodoOS.veic.prop.NroRegEstadual);
  AssertEquals('CTe.infCteNorm.rodoOS.prop.xNome', 'Proprietario', FCTe.infCteNorm.rodoOS.veic.prop.xNome);
  AssertEquals('CTe.infCteNorm.rodoOS.prop.IE', '123456789012345', FCTe.infCteNorm.rodoOs.veic.prop.IE);
  AssertEquals('CTe.infCteNorm.rodoOS.prop.UF', 'SP', FCTe.infCTeNorm.rodoOS.veic.prop.UF);
  AssertEquals('CTe.infCteNorm.rodoOS.prop.tpProp', '0', TpPropToStr(FCTe.infCTeNorm.rodoOS.veic.prop.tpProp));
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_cobr;
begin
  //fat
  AssertEquals('CTe.infCteNorm.cobr.fat.nFat', '1', FCTe.infCTeNorm.cobr.fat.nFat);
  AssertEquals('CTe.infCteNorm.cobr.fat.vOrig', 10.00, FCTe.infCTeNorm.cobr.fat.vOrig);
  AssertEquals('CTe.infCteNorm.cobr.fat.vDesc', 2.00, FCTe.infCTeNorm.cobr.fat.vDesc);
  AssertEquals('CTe.infCteNorm.cobr.fat.vLiq', 8.00, FCTe.infCTeNorm.cobr.fat.vLiq);
  AssertEquals('CTe.infCteNorm.cobr.dup.Count', 1, FCTe.infCteNorm.cobr.dup.Count);
  AssertEquals('CTe.infCteNorm.cobr.dup.nDup', '1', FCTe.infCteNorm.cobr.dup[0].nDup);
  AssertEquals('CTe.infCteNorm.cobr.dup.dVenc', EncodeDataHora('26/01/2024', 'DD/MM/YYYY'), FCTe.infCteNorm.cobr.dup[0].dVenc);
  AssertEquals('CTe.infCteNorm.cobr.dup.vDup', 8.00, FCTe.infCTeNorm.cobr.dup[0].vDup);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infGTVe;
begin
  AssertEquals('CTe.infCteNorm.infGTVe.Count', 1, FCTe.infCTeNorm.infGTVe.Count);
  AssertEquals('CTe.infCteNorm.infGTVe[0].chCTe', '35240118760500000000640010000000011773821899', FCTe.infCTeNorm.infGTVe[0].chCTe);
  AssertEquals('CTe.infCteNorm.infGTVe[0].Comp.Count', 1, FCTe.infCteNorm.infGTVe[0].Comp.Count);
  AssertEquals('CTe.infCteNorm.infGTVe[0].comp[0].tpComp', '1', tpCompToStr(FCTe.infCTeNorm.infGTVe[0].Comp[0].tpComp));
  AssertEquals('CTe.infCteNorm.infGTVe[0].comp[0].vComp', 8.00, FCTe.infCTeNorm.infGTVe[0].Comp[0].vComp);
  AssertEquals('CTe.infCteNorm.infGTVe[0].comp[0].xComp', 'FRETE', FCTe.infCTeNorm.infGTVe[0].Comp[0].xComp);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_autXML;
begin
  AssertEquals('CTe.autXML.Count', 1, FCTe.autXML.Count);
  AssertEquals('CTe.autXML[0].CNPJCPF', '11111112222222', FCTe.autXML[0].CNPJCPF);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infRespTec;
begin
  AssertEquals('CTe.infRespTec.CNPJ', '11111111111111', FCTe.infRespTec.CNPJ);
  AssertEquals('CTe.infRespTec.xContato', 'XContato', FCTe.infRespTec.xContato);
  AssertEquals('CTe.infRespTec.email', 'email@mail', FCTe.infRespTec.email);
  AssertEquals('CTe.infRespTec.fone', '11111111', FCTe.infRespTec.fone);
end;

procedure TACBrCTeXmlReaderReadingTests_CTeOS_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infCTeSupl;
begin
  AssertEquals('CTe.infCTeSupl.qrCodCTe', 'https://homologacao.nfe.fazenda.sp.gov.br/CTeConsulta/qrCode?chCTe=35240118760500000000670010000000011434778690&tpAmb=2', FCTe.infCTeSupl.qrCodCTe);
end;

initialization
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.XMLCompleto', TACBrCTeXmlReaderReadingTests_CTeOS_Ver400);

end.

