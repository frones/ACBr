unit ACBrCTeXmlReaderReadingTests_CTe_Ver400;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ACBrCTeXmlHandler, ACBrTests.Util, pcteCTe;

type


  { TACBrCTeXmlReaderReadingTests_GrupoIdDocAnt_ver400 }

  TACBrCTeXmlReaderReadingTests_GrupoIdDocAnt_ver400 = class(TTestCase)
  published
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_idDocAntPap;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_idDocAntEle;
  end;

  { TACBrCTeXmlReaderReadingTests_GrupoInfDoc_ver400 }

  TACBrCTeXmlReaderReadingTests_GrupoInfDoc_ver400 = class(TTestCase)
  published
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_InfNFe;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_InfNF;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_InfOutros;
  end;

  { TACBrCTeXmlReaderReadingTests_GruposICMS_ver400 }

  TACBrCTeXmlReaderReadingTests_GruposICMS_ver400 = class(TTestCase)
  published
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS00;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS20;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS45;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS60;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS90;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ICMSOutraUF;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ICMSSN;
  end;

  { TACBrCTeXmlReaderReadingTests_GruposModais_Ver400 }

  TACBrCTeXmlReaderReadingTests_GruposModais_Ver400 = class(TTestCase)
  published
    procedure LerXML_LeuCorretamenteOsDados_ModalRodoviario;
    procedure LerXML_LeuCorretamenteOsDados_ModalAereo;
    procedure LerXML_LeuCorretamenteOsDados_ModalFerroviario;
    procedure LerXML_LeuCorretamenteOsDados_ModalAquaviario;
    procedure LerXML_LeuCorretamenteOsDados_ModalDutoviario;
    procedure LerXML_LeuCorretamenteOsDados_Multimodal;
  end;

  { TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver400 }

  TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver400 = class(TTestCase)
  published
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infCTeComp;
  end;

  { TACBrCTeXmlReaderReadingTests_CTe_Ver400 }

  TACBrCTeXmlReaderReadingTests_CTe_Ver400 = class(TTestCase)
  private
    FCTe: TCTe;
    FCTeReader: TCTeXmlReader;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LerXML_LeuCorretamenteOsAtributosDoGrupo_InfCTe;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ide;
    procedure LerXML_LeuCorretamentoOsDadosDoGrupo_toma3;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_toma4;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_compl;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_emit;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_rem;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_exped;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_receb;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_dest;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_vPrest;
    procedure LerXML_LeuCorretemanteOsDadosDoGrupo_imp;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infCarga;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infNFe;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_emiDocAnt;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_rodo;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_veicNovos;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_cobr;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_autXML;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infRespTec;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infCTeSupl;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_Signature;
  end;

implementation

uses
  ACBrCTeTestConsts, ACBrUtil.DateTime, pcnConversao, pcteConversaoCTe;

{ TACBrCTeXmlReaderReadingTests_GrupoIdDocAnt_ver400 }

procedure TACBrCTeXmlReaderReadingTests_GrupoIdDocAnt_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_idDocAntPap;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTe_idDocAntPap);
    FCTeReader.LerXML;

    AssertEquals('CTe.infCteNorm.docAnt.emiDocAnt.Count', 1, FCTe.infCteNorm.docAnt.emiDocAnt.Count);
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt.Count', 1, FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt.Count);
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap.Count', 1, FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap.Count);
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap[0].tpDoc', '07' , TpDocumentoAnteriorToStr(FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap[0].tpDoc));
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap[0].serie', '1', FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap[0].serie);
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap[0].nDoc', '123', FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap[0].nDoc);
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap[0].dEmi', EncodeDataHora('10/12/2020', 'dd/mm/yyyy'), FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntPap[0].dEmi);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GrupoIdDocAnt_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_idDocAntEle;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTe_idDocAntEle);
    FCTeReader.LerXML;

    AssertEquals('CTe.infCteNorm.docAnt.emiDocAnt.Count', 1, FCTe.infCteNorm.docAnt.emiDocAnt.Count);
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt.Count', 1, FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt.Count);
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle.Count', 1, FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle.Count);
    AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle[0].chCTe', '33190100127817000125650080000000581000384589' ,FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle[0].chCTe);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

{ TACBrCTeXmlReaderReadingTests_GrupoInfDoc_ver400 }

procedure TACBrCTeXmlReaderReadingTests_GrupoInfDoc_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_InfNFe;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_InfNFe);
    FCTeReader.LerXml;

    AssertEquals('CTe.infCteNorm.infDoc.infNFe.Count', 1, FCTe.infCTeNorm.infDoc.infNFe.Count);

    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].chave', '42210117089484000190550110000091001371413248', FCTe.infCTeNorm.infDoc.infNFe[0].chave);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].PIN', '98754321', FCTe.infCteNorm.infDoc.infNFe[0].PIN);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].dPrev', EncodeDataHora('29/02/2024','dd/mm/yyyy'),  FCTe.infCteNorm.infDoc.infNFe[0].dPrev);

    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[0].infUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidCarga[0].tpUnidCarga', '4', UnidCargaToStr(FCTe.infCTeNorm.infDoc.InfNFe[0].infUnidCarga[0].tpUnidCarga));
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidCarga[0].idUnidCarga', 'CX1', FCTe.infCTeNorm.infDoc.InfNFe[0].infUnidCarga[0].idUnidCarga);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidCarga[0].qtdRat', 10, FCTe.infCTeNorm.infDoc.infNFe[0].infunidCarga[0].qtdRat);

    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidCarga[0].lacUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.InfNFe[0].infUnidCarga[0].lacUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidCarga[0].lacUnidCarga[0].nLacre', '1', FCTe.infCTeNorm.infDoc.InfNFe[0].infUnidCarga[0].lacUnidCarga[0].nLacre);

    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].tpUnidTransp', '7', UnidTranspToStr(FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].tpUnidTransp));
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].idUnidTransp', 'TP1', FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].idUnidTransp);

    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].lacUnidTransp.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].lacUnidTransp.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].lacUnidTransp[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].lacUnidTransp[0].nLacre);

    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infUnidCarga[0].tpUnidCarga', '4', UnidCargaToStr(FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infunidCarga[0].tpUnidCarga));
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infUnidCarga[0].idUnidCarga', 'T1', FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infunidCarga[0].idUnidCarga);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infUnidCarga[0].qtdRat', 10, FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infunidCarga[0].qtdRat);

    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infunidCarga[0].lacUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infNFe[0].infUnidTransp[0].infunidCarga[0].lacUnidCarga[0].nLacre);
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GrupoInfDoc_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_InfNF;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_InfNF);
    FCTeReader.LerXML;

    AssertEquals('CTe.infCTeNorm.infDoc.infNF.Count', 1, FCTe.infCTeNorm.infDoc.infNF.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].nRoma', '1', FCTe.infCTeNorm.infDoc.infNF[0].nRoma);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].nPed', '2', FCTe.infCTeNorm.infDoc.infNF[0].nPed);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].modelo', '04', ModeloNFToStr(FCTe.infCTeNorm.infDoc.infNF[0].modelo));
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].serie', '1', FCTe.infCTeNorm.infDoc.infNF[0].serie);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].nDoc', '1', FCTe.infCTeNorm.infDoc.infNF[0].nDoc);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].dEmi', EncodeDataHora('31/01/2024', 'dd/mm/yyyy'), FCTe.infCTeNorm.infDoc.infNF[0].dEmi);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].vBC', 1000, FCTe.infCTeNorm.infDoc.infNF[0].vBC);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].vICMS', 100, FCTe.infCTeNorm.infDoc.infNF[0].vICMS);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].vBCCST', 1000, FCTe.infCTeNorm.infDoc.infNF[0].vBCST);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].vST', 100, FCTe.infCTeNorm.infDoc.infNF[0].vST);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].vProd', 1000, FCTe.infCTeNorm.infDoc.infNF[0].vProd);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].vNF', 1000, FCTe.infCTeNorm.infDoc.infNF[0].vNF);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].nCFOP', 5405, FCTe.infCTeNorm.infDoc.infNF[0].nCFOP);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].nPeso', 100, FCTe.infCTeNorm.infDoc.infNF[0].nPeso);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].PIN', '123456', FCTe.infCTeNorm.infDoc.infNF[0].PIN);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].dPrev', EncodeDataHora('31/01/2024', 'dd/mm/yyyy'), FCTe.infCTeNorm.infDoc.infNF[0].dPrev);

    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNF[0].infUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].tpUnidCarga', '1', UnidCargaToStr(FCTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].tpUnidCarga));
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].idUnidCarga', 'C01', FCTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].idUnidCarga);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].qtdRat', 100, FCTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].qtdRat);

    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].lacUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].lacUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].lacUnidCarga[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infNF[0].infUnidCarga[0].lacUnidCarga[0].nLacre);

    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp.Count', 1, FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].tpUnidTransp', '1', UnidTranspToStr(FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].tpUnidTransp));
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].idUnidTransp', '1', FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].idUnidTransp);

    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].lacUnidTransp.Count', 1, FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].lacUnidTransp.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].lacUnidTransp[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].lacUnidTransp[0].nLacre);

    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].tpUnidCarga', '1', UnidCargaToStr(FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].tpUnidCarga));
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].idUnidCarga', 'C01', FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].idUnidCarga);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].qtdRat', 100, FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].qtdRat);

    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infNF[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga[0].nLacre);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GrupoInfDoc_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_InfOutros;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeREader.CarregarArquivo(XML_CTE_InfOutros);
    FCTeReader.LerXML;

    AssertEquals('CTe.infCTeNorm.infDoc.infOutros.Count', 1, FCTe.infCTeNorm.infDoc.infOutros.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].tpDoc', '00' , TpDocumentoToStr(FCTe.infCTeNorm.infDoc.infOutros[0].tpDoc));
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].descOutros', 'Teste', FCTe.infCTeNorm.infDoc.infOutros[0].descOutros);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].nDoc', '123', FCTe.infCTeNorm.infDoc.infOutros[0].nDoc);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].dEmi', EncodeDataHora('31/01/2024', 'dd/mm/yyyy'), FCTe.infCTeNorm.infDoc.infOutros[0].dEmi);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].vDocFisc', 1000, FCTe.infCTeNorm.infDoc.infOutros[0].vDocFisc);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].dPrev', EncodeDataHora('31/01/2024', 'dd/mm/yyyy'), FCTe.infCTeNorm.infDoc.infOutros[0].dPrev);

    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].tpUnidCarga', '3',UnidCargaToStr(FCTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].tpUnidCarga));
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].idUnidCarga', 'CG', FCTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].idUnidCarga);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].qtdRat', 10, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].qtdRat);

    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].lacUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].lacUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].lacUnidCarga[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infOutros[0].infUnidCarga[0].lacUnidCarga[0].nLacre);

    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp.Count', 1, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].tpUnidTransp', '2', UnidTranspToStr(FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].tpUnidTransp));
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].idUnidTransp', 'CT', FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].idUnidTransp);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].qtdRat', 11, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].qtdRat);

    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].lacUnidTransp.Count', 1, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].lacUnidTransp.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].lacUnidTransp[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].lacUnidTransp[0].nLacre);

    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].tpUnidCarga', '3', UnidCargaToStr(FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].tpUnidCarga));
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].idUnidCarga', 'CG-1', FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].idUnidCarga);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].qtdRat', 12, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].qtdRat);

    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga.Count);
    AssertEquals('CTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga[0].nLacre', '12', FCTe.infCTeNorm.infDoc.infOutros[0].infUnidTransp[0].infUnidCarga[0].lacUnidCarga[0].nLacre);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

{ TACBrCTeXmlReaderReadingTests_GruposICMS_ver400 }

procedure TACBrCTeXmlReaderReadingTests_GruposICMS_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS00;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_ICMS00);
    FCTeReader.LerXml;

    AssertEquals('CTe.Imp.ICMS.SituTrib', '00', CSTICMSToStr(FCTe.imp.ICMS.SituTrib));
    AssertEquals('CTe.imp.ICMS.ICMS00.CST', '00', CSTICMSToStr(FCTe.imp.ICMS.ICMS00.CST));
    AssertEquals('CTe.imp.ICMS.ICMS00.vBC', 1000.00, FCTe.imp.ICMS.ICMS00.vBC);
    AssertEquals('CTe.imp.ICMS.ICMS00.pICMS', 10.00, FCTe.imp.ICMS.ICMS00.pICMS);
    AssertEquals('CTe.imp.ICMS.ICMS00.vICMS', 100.00, FCTe.imp.ICMS.ICMS00.vICMS);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposICMS_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS20;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_ICMS20);
    FCTeReader.LerXml;

    AssertEquals('CTe.Imp.ICMS.SituTrib', '20', CSTICMSToStr(FCTe.imp.ICMS.SituTrib));
    AssertEquals('CTe.imp.ICMS.ICMS20.CST', '20', CSTICMSToStr(FCTe.imp.ICMS.ICMS20.CST));
    AssertEquals('CTe.imp.ICMS.ICMS20.pRedBC', 5.00, FCTe.imp.ICMS.ICMS20.pRedBC);
    AssertEquals('CTe.imp.ICMS.ICMS20.vBC', 1000.00, FCTe.imp.ICMS.ICMS20.vBC);
    AssertEquals('CTe.imp.ICMS.ICMS20.pICMS', 10.00, FCTe.imp.ICMS.ICMS20.pICMS);
    AssertEquals('CTe.imp.ICMS.ICMS20.vICMS', 95.00, FCTe.imp.ICMS.ICMS20.vICMS);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposICMS_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS45;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_ICMS45);
    FCTeReader.LerXml;

    AssertEquals('CTe.Imp.ICMS.SituTrib', '40', CSTICMSToStr(FCTe.imp.ICMS.SituTrib));
    AssertEquals('CTe.imp.ICMS.ICMS45.CST', '40' , CSTICMSToStr(FCTe.imp.ICMS.ICMS45.CST));

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposICMS_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS60;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_ICMS60);
    FCTeReader.LerXml;

    AssertEquals('CTe.Imp.ICMS.SituTrib', '60', CSTICMSToStr(FCTe.imp.ICMS.SituTrib));
    AssertEquals('CTe.imp.ICMS.ICMS60.CST', '60', CSTICMSToStr(FCTe.imp.ICMS.ICMS60.CST));
    AssertEquals('CTe.imp.ICMS.ICMS60.vBCSTRet', 1000, FCTe.imp.ICMS.ICMS60.vBCSTRet);
    AssertEquals('CTe.imp.ICMS.ICMS60.vICMSSTRet', 100, FCTe.imp.ICMS.ICMS60.vICMSSTRet);
    AssertEquals('CTe.imp.ICMS.ICMS60.pICMSSTRet', 10, FCTe.imp.ICMS.ICMS60.pICMSSTRet);
    AssertEquals('CTe.imp.ICMS.ICMS60.vCred', 50, FCTe.imp.ICMS.ICMS60.vCred);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposICMS_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ICMS90;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_ICMS90);
    FCTeReader.LerXml;

    AssertEquals('CTe.Imp.ICMS.SituTrib', '90', CSTICMSToStr(FCTe.imp.ICMS.SituTrib));
    AssertEquals('CTe.imp.ICMS.ICMS90.CST', '90', CSTICMSToStr(FCTe.Imp.ICMS.ICMS90.CST));
    AssertEquals('CTe.imp.ICMS.ICMS90.pRedBC', 10, FCTe.imp.ICMS.ICMS90.pRedBC);
    AssertEquals('CTe.imp.ICMS.ICMS90.vBC', 1000, FCTe.Imp.ICMS.ICMS90.vBC);
    AssertEquals('CTe.imp.ICMS.ICMS90.pICMS', 10, FCTe.imp.ICMS.ICMS90.pICMS);
    AssertEquals('CTe.imp.ICMS.ICMS90.vICMS', 100, FCTe.imp.ICMS.ICMS90.vICMS);
    AssertEquals('CTe.imp.ICMS.ICMS90.vCred', 50, FCTe.imp.ICMS.ICMS90.vCred);
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposICMS_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ICMSOutraUF;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_ICMSOUTRAUF);
    FCTeReader.LerXml;

    AssertEquals('CTe.Imp.ICMS.SituTrib', '90', CSTICMSToStr(FCTe.Imp.ICMS.SituTrib));
    AssertEquals('CTe.Imp.ICMS.ICMSOutraUF.CST', '90', CSTICMSToStr(FCTe.Imp.ICMS.ICMSOutraUF.CST));
    AssertEquals('CTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF', 10, FCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF);
    AssertEquals('CTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF', 1000, FCTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF);
    AssertEquals('CTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF', 10, FCTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF);
    AssertEquals('CTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF', 100, FCTe.imp.ICMS.ICMSOutraUF.vICMSOutraUF);
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposICMS_ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ICMSSN;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    FCTeReader.CarregarArquivo(XML_CTE_ICMSSN);
    FCTeReader.LerXml;

    //A função de conversão CSTICMSTOSTR converte para SN, mas ao gerar usa 90.
    //AssertEquals('CTe.Imp.ICMS.SituTrib', '90', CSTICMSToStr(FCTe.Imp.ICMS.SituTrib));
    //AssertEquals('CTe.Imp.ICMS.ICMSSN.CST', '90', CSTICMSToStr(FCTe.Imp.ICMS.ICMSSN.CST));
    AssertEquals('CTe.Imp.ICMS.ICMSSN.IndSN', 1, FCTe.Imp.ICMS.ICMSSN.indSN);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

{ TACBrCTeXmlReaderReadingTests_GruposModais_Ver400 }

procedure TACBrCTeXmlReaderReadingTests_GruposModais_Ver400.LerXML_LeuCorretamenteOsDados_ModalRodoviario;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
     FCTeReader.CarregarArquivo(XML_CTE_MODALRODOVIARIO_VERSAO400);
     FCTeReader.LerXML;

     AssertEquals('CTe.infCTeNorm.rodo.RNTRC', '12345678', FCTe.infCTeNorm.rodo.RNTRC);
     AssertEquals('CTe.infCTeNorm.rodo.occ.Count', 2, FCTe.infCTeNorm.rodo.occ.Count);
     AssertEquals('CTe.infCTeNorm.rodo.occ[0].serie', '001', FCTe.infCTeNorm.rodo.occ[0].serie);
     AssertEquals('CTe.infCTeNorm.rodo.occ[0].nOcc', 1, FCTe.infCTeNorm.rodo.occ[0].nOcc);
     AssertEquals('CTe.infCTeNorm.rodo.occ[0].dEmi', EncodeDataHora('16/01/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.rodo.occ[0].dEmi);
     AssertEquals('CTe.infCtrNorm.rodo.occ[0].emiOcc.CNPJ', '12345678000123', FCTe.infCTeNorm.rodo.occ[0].emiOcc.CNPJ);
     AssertEquals('CTe.infCtrNorm.rodo.occ[0].emiOcc.cInt', '501', FCTe.infCTeNorm.rodo.occ[0].emiOcc.cInt);
     AssertEquals('CTe.infCtrNorm.rodo.occ[0].emiOcc.IE', '1234567', FCTe.infCTenorm.rodo.occ[0].emiOcc.IE);
     AssertEquals('CTe.infCtrNorm.rodo.occ[0].emiOcc.UF', 'SP', FCTe.infCTeNorm.rodo.occ[0].emiOcc.UF);
     AssertEquals('CTe.infCtrNorm.rodo.occ[0].emiOcc.fone', '22334455', FCTe.infCteNorm.rodo.occ[0].emiOcc.fone);
     AssertEquals('CTe.infCTeNorm.rodo.occ[1].serie', '002', FCTe.infCTeNorm.rodo.occ[1].serie);
     AssertEquals('CTe.infCTeNorm.rodo.occ[1].nOcc', 2, FCTe.infCTeNorm.rodo.occ[1].nOcc);
     AssertEquals('CTe.infCTeNorm.rodo.occ[1].dEmi', EncodeDataHora('16/01/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.rodo.occ[1].dEmi);
     AssertEquals('CTe.infCtrNorm.rodo.occ[1].emiOcc.CNPJ', '12345678000321', FCTe.infCTeNorm.rodo.occ[1].emiOcc.CNPJ);
     AssertEquals('CTe.infCtrNorm.rodo.occ[1].emiOcc.cInt', '502', FCTe.infCTeNorm.rodo.occ[1].emiOcc.cInt);
     AssertEquals('CTe.infCtrNorm.rodo.occ[1].emiOcc.IE', '7654321', FCTe.infCTenorm.rodo.occ[1].emiOcc.IE);
     AssertEquals('CTe.infCtrNorm.rodo.occ[1].emiOcc.UF', 'SP', FCTe.infCTeNorm.rodo.occ[1].emiOcc.UF);
     AssertEquals('CTe.infCtrNorm.rodo.occ[1].emiOcc.fone', '55443322', FCTe.infCteNorm.rodo.occ[1].emiOcc.fone);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposModais_Ver400.LerXML_LeuCorretamenteOsDados_ModalAereo;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
     FCTeReader.CarregarArquivo(XML_CTE_MODALAEREO_VERSAO400);
     FCTeReader.LerXML;

     AssertEquals('CTe.infCteNorm.aereo.nMinu', 000000001, FCTe.infCTeNorm.aereo.nMinu);
     AssertEquals('CTe.infCteNorm.aereo.nOCA', '01', FCTe.infCTeNorm.aereo.nOCA);
     AssertEquals('CTe.infCteNorm.aereo.dPrevAereo', EncodeDataHora('17/01/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.aereo.dPrevAereo);
     AssertEquals('CTe.infCteNorm.aereo.natCarga.xDime', '1234X1234X1234', FCTe.infCteNorm.aereo.natCarga.xDime);
     AssertEquals('CTe.infCteNorm.aereo.natCarga.cInfManu.Count', 2, FCTe.infCteNorm.aereo.natCarga.cInfManu.Count);
     AssertEquals('CTe.infCteNorm.aereo.natCarga.cInfManu[0].nInfManu', '01', TpInfManuToStr(FCTe.infCteNorm.aereo.natCarga.cInfManu[0].nInfManu));
     AssertEquals('CTe.infCteNorm.aereo.natCarga.cInfManu[1].nInfManu', '02', TpInfManuToStr(FCTe.infCteNorm.aereo.natCarga.cInfManu[1].nInfManu));
     AssertEquals('CTe.infCteNorm.aereo.tarifa.CL', 'G', FCTe.infCteNorm.aereo.tarifa.CL);
     AssertEquals('CTe.infCteNorm.aereo.tarifa.cTar', '123', FCTe.infCteNorm.aereo.tarifa.cTar);
     AssertEquals('CTe.infCteNorm.aereo.tarifa.vTar', 10.00, FCTe.infCteNorm.aereo.tarifa.vTar);

     AssertEquals('CTe.infCteNorm.peri.Count', 2, FCTe.infCteNorm.peri.Count);
     AssertEquals('CTe.infCteNorm.peri[0].nONU', '1234', FCTe.infCteNorm.peri[0].nONU);
     AssertEquals('CTe.infCteNorm.peri[0].qTotEmb', '1', FCTe.infCteNorm.peri[0].qTotEmb);
     AssertEquals('CTe.infCteNorm.peri[0].qTotProd', '1', FCTe.infCteNorm.peri[0].qTotProd);
     AssertEquals('CTe.infCteNorm.peri[0].uniAP', '1', UniMedToStr(FCTe.infCteNorm.peri[0].uniAP));

     AssertEquals('CTe.infCteNorm.peri[1].nONU', '5678', FCTe.infCteNorm.peri[1].nONU);
     AssertEquals('CTe.infCteNorm.peri[1].qTotEmb', '2', FCTe.infCteNorm.peri[1].qTotEmb);
     AssertEquals('CTe.infCteNorm.peri[1].qTotProd', '2', FCTe.infCteNorm.peri[1].qTotProd);
     AssertEquals('CTe.infCteNorm.peri[1].uniAP', '1', UniMedToStr(FCTe.infCteNorm.peri[1].uniAP));

  finally
    FCTe.Free;
    FCTeReader.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposModais_Ver400.LerXML_LeuCorretamenteOsDados_ModalFerroviario;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
     FCTeReader.CarregarArquivo(XML_CTE_MODALFERROVIARIO_VERSAO400);
     FCTeReader.LerXml;

     AssertEquals('CTe.infCTeNorm.ferrov.tpTraf', '1', TpTrafegoToStr(FCTe.infCTeNorm.ferrov.tpTraf));
     AssertEquals('CTe.infCTeNorm.ferrov.trafMut.respFat', '1', TrafegoMutuoToStr(FCTe.infCTeNorm.ferrov.trafMut.respFat));
     AssertEquals('CTe.infCTeNorm.ferrov.trafMut.ferrEmi', '1', TrafegoMutuoToStr(FCTe.infCTeNorm.ferrov.trafMut.ferrEmi));
     AssertEquals('CTe.infCTeNorm.ferrov.trafMut.chCTeFerroOrigem', '33240118760500000000570000000005051984519606', FCTe.infCTeNorm.ferrov.trafMut.chCTeFerroOrigem);
     AssertEquals('CTe.infCTeNorm.ferrov.vFrete', 10.00, FCTe.infCTeNorm.ferrov.vFrete);
     AssertEquals('CTe.infCTeNorm.ferrov.Count', 2, FCTe.infCTeNorm.ferrov.ferroEnv.Count);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].CNPJ', '11111111111111', FCTe.infCTeNorm.ferrov.ferroEnv[0].CNPJ);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].cInt', '1', FCTe.infCTeNorm.ferrov.ferroEnv[0].cInt);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].IE', '12345678901234', FCTe.infCTeNorm.ferrov.ferroEnv[0].IE);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].xNome', 'Ferro1', FCTe.infCTeNorm.ferrov.ferroEnv[0].xNome);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.xLgr', 'Lgr.Ferro1', FCTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.xLgr);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.nro', '1', FCTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.nro);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.xCpl', 'Cpl.Ferro1', FCTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.xCpl);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.xBairro', 'Bairro.Ferro1', FCTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.xBairro);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.cMun', 3301207, FCTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.cMun);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.xMun', 'Carmo', FCTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.xMun);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.CEP', 11111111, FCTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.CEP);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.UF', 'RJ', FCTe.infCTeNorm.ferrov.ferroEnv[0].enderFerro.UF);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].CNPJ', '22222222222222', FCTe.infCTeNorm.ferrov.ferroEnv[1].CNPJ);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].cInt', '2', FCTe.infCTeNorm.ferrov.ferroEnv[1].cInt);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].IE', '43210987654321', FCTe.infCTeNorm.ferrov.ferroEnv[1].IE);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].xNome', 'Ferro2', FCTe.infCTeNorm.ferrov.ferroEnv[1].xNome);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.xLgr', 'Lgr.Ferro2', FCTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.xLgr);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.nro', '2', FCTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.nro);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.xCpl', 'Cpl.Ferro2', FCTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.xCpl);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.xBairro', 'Bairro.Ferro2', FCTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.xBairro);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.cMun', 3301207, FCTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.cMun);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.xMun', 'Carmo', FCTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.xMun);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.CEP', 22222222, FCTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.CEP);
     AssertEquals('CTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.UF', 'RJ', FCTe.infCTeNorm.ferrov.ferroEnv[1].enderFerro.UF);

     AssertEquals('CTe.infCTeNorm.ferrov.fluxo', '1234567890', FCTe.infCTeNorm.ferrov.fluxo);
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposModais_Ver400.LerXML_LeuCorretamenteOsDados_ModalAquaviario;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
     FCTeReader.CarregarArquivo(XML_CTE_MODALAQUAVIARIO_VERSAO400);
     FCTeReader.LerXML;

     AssertEquals('CTe.infCTeNorm.aquav.vPrest', 10.00, FCTe.infCTeNorm.aquav.vPrest);
     AssertEquals('CTe.infCTeNorm.aquav.vAFRMM', 11.00, FCTe.infCTeNorm.aquav.vAFRMM);
     AssertEquals('CTe.infCTeNorm.aquav.xNavio', 'Titanic', FCTe.infCTeNorm.aquav.xNavio);
     AssertEquals('CTe.infCTeNorm.aquav.balsa.Count', 2, FCTe.infCTeNorm.aquav.balsa.Count);
     AssertEquals('CTe.infCTeNorm.aquav.balsa.xBalsa', 'ABC', FCTe.infCTeNorm.aquav.balsa[0].xBalsa);
     AssertEquals('CTe.infCTeNorm.aquav.balsa.xBalsa', 'DEF', FCTe.infCTeNorm.aquav.balsa[1].xBalsa);
     AssertEquals('CTe.infCTeNorm.aquav.nViag', '1', FCTe.infCTeNorm.aquav.nViag);
     AssertEquals('CTe.infCTeNorm.aquav.direc', 'S', TpDirecaoToStr(FCTe.infCTeNorm.aquav.direc));
     AssertEquals('CTe.infCTeNorm.aquav.irin', 'S', FCTe.infCTeNorm.aquav.irin);
     AssertEquals('CTe.infCTeNorm.aquav.detCount.Count', 2, FCTe.infCTeNorm.aquav.detCont.Count);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[0].nCount', '123', FCTe.infCTeNorm.aquav.detCont[0].nCont);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[0].Lacre.Count', 2, FCTe.infCTeNorm.aquav.detCont[0].Lacre.Count);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[0].Lacre[0].nLacre', '1', FCTe.infCTeNorm.aquav.detCont[0].Lacre[0].nLacre);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[0].Lacre[1].nLacre', '2', FCTe.infCTeNorm.aquav.detCont[0].Lacre[1].nLacre);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[0].infDoc.InfNFe.Count', 1, FCTe.infCTeNorm.aquav.detCont[0].infDoc.infNFe.Count);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[0].infDoc.infNFe[0].chave', '51240100048115940100559200000000011873082365', FCTe.infCTeNorm.aquav.detCont[0].infDoc.infNFe[0].chave);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[0].infDoc.infNFe[0].unidRat', 1.00, FCTe.infCTeNorm.aquav.detCont[0].infDoc.infNFe[0].unidRat);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[1].nCount', '123', FCTe.infCTeNorm.aquav.detCont[1].nCont);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[1].Lacre.Count', 2, FCTe.infCTeNorm.aquav.detCont[1].Lacre.Count);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[1].Lacre[0].nLacre', '1', FCTe.infCTeNorm.aquav.detCont[1].Lacre[0].nLacre);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[1].Lacre[1].nLacre', '2', FCTe.infCTeNorm.aquav.detCont[1].Lacre[1].nLacre);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[1].infDoc.infNF.Count', 1, FCTe.infCTeNorm.aquav.detCont[1].infDoc.infNF.Count);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[1].infDoc.infNF[0].serie', '1', FCTe.infCTeNorm.aquav.detCont[1].infDoc.infNF[0].serie);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[1].infDoc.infNF[0].nDoc', '123', FCTe.infCTeNorm.aquav.detCont[1].infDoc.infNF[0].nDoc);
     AssertEquals('CTe.infCTeNorm.aquav.detCount[1].infDoc.infNF[0].unidRat', 1.00, FCTe.infCTeNorm.aquav.detCont[1].infDoc.infNF[0].unidRat);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposModais_Ver400.LerXML_LeuCorretamenteOsDados_ModalDutoviario;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
     FCTeReader.CarregarArquivo(XML_CTE_MODALDUTOVIARIO_VERSAO400);
     FCTeReader.LerXML;

     AssertEquals('CTe.infCTeNorm.duto.vTar', 10.000000, FCTe.infCTeNorm.duto.vTar);
     AssertEquals('CTe.infCTeNorm.duto.dIni', EncodeDataHora('15/01/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.duto.dIni);
     AssertEquals('CTe.infCTeNorm.duto.dFim', EncodeDataHora('20/01/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.duto.dFim);
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderReadingTests_GruposModais_Ver400.LerXML_LeuCorretamenteOsDados_Multimodal;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
     FCTeReader.CarregarArquivo(XML_CTE_MODALMULTIMODAL_VERSAO400);
     FCTeReader.LerXml;

     AssertEquals('CTe.infCTeNorm.multimodal.COTM', '1234', FCTe.infCTeNorm.multimodal.COTM);
     AssertEquals('CTe.infCTeNorm.multimodal.indNegociavel', '0', indNegociavelToStr(FCTe.infCTeNorm.multimodal.indNegociavel));
     AssertEquals('CTe.infCTeNrom.multimodal.xSeg', 'Seguradora', FCTe.infCTeNorm.multimodal.xSeg);
     AssertEquals('CTe.infCTeNorm.multimodal.CNPJ', '44444444444444', FCTe.infCTeNorm.multimodal.CNPJ);
     AssertEquals('CTe.infCTeNorm.multimodal.nApol', '12345678', FCTe.infCTeNorm.multimodal.nApol);
     AssertEquals('CTe.infCTeNorm.multimodal.nAver', '654321', FCTe.infCTeNorm.multimodal.nAver);
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

{ TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver400 }

procedure TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infCTeComp;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
     FCTeReader.CarregarArquivo(XML_CTE_COMPLEMENTAR_VERSAO400);
     FCTeReader.LerXml;

     //É a mesma estrutura do XML comum, por isso vou testar somente o grupo especifico
     AssertEquals('CTe.infCteComp10.Count', 3, FCTe.infCteComp10.Count);
     AssertEquals('CTe.infCteComp10[0].chCte', '35231118760500000000570010000000011946904021', FCTe.infCteComp10[0].chCTe);
     AssertEquals('CTe.infCteComp10[1].chCte', '35231118760500000000570010000000011333049526', FCTe.infCteComp10[1].chCTe);
     AssertEquals('CTe.infCteComp10[2].chCte', '35231118760500000000570010000000011390053898', FCTe.infCteComp10[2].chCte);

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

{ TACBrCTeXmlReaderReadingTests_CTe_Ver400 }

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.SetUp;
begin
  inherited SetUp;
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  FCTeReader.CarregarArquivo(XML_CTE_NORMAL_VERSAO400);
  FCTeReader.LerXml;
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.TearDown;
begin
  FCTeReader.Free;
  FCTe.Free;
  inherited TearDown;
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsAtributosDoGrupo_InfCTe;
begin
  AssertEquals('CTe.InfCTe.Versao', 4, FCTe.infCTe.versao);
  AssertEquals('CTe.infCTe.Id', 'CTe35240118760500000000570010000000011121942170', FCTe.infCTe.Id);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ide;
begin
  AssertEquals('CTe.ide.cUF', 35, FCTe.ide.cUF);
  AssertEquals('CTe.ide.cCT', 12194217, FCTe.ide.cCT);
  AssertEquals('CTe.ide.CFOP', 6932, FCTe.ide.CFOP);
  AssertEquals('CTe.ide.natOP', 'PRESTACAO SERVICO', FCTe.ide.natOp);
  AssertEquals('CTe.ide.mod', 57, FCTe.ide.modelo);
  AssertEquals('CTe.ide.serie', 1, FCTe.ide.serie);
  AssertEquals('CTe.ide.nCT', 1, FCTe.ide.nCT);
  AssertEquals('CTe.ide.dhEmi', EncodeDataHora('12/01/2024 17:23:09', 'dd/mm/yyyy hh:nn:ss'), FCTe.ide.dhEmi);
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
  AssertEquals('CTe.ide.tpServ', '0', TpServPagToStr(FCTe.ide.tpServ));
  AssertEquals('CTe.ide.cMunIni', 3119401, FCTe.ide.cMunIni);
  AssertEquals('CTe.ide.xMunIni', 'CORONEL FABRICIANO', FCTe.ide.xMunIni);
  AssertEquals('CTe.ide.UFIni', 'MG', FCTe.ide.UFIni);
  AssertEquals('CTe.ide.cMunFim', 2900207, FCTe.ide.cMunFim);
  AssertEquals('CTe.ide.xMunFim', 'ABARE', FCTe.ide.xMunFim);
  AssertEquals('CTe.ide.UFFim', 'BA', FCTe.ide.UFFim);
  AssertEquals('CTe.ide.retira', '0', TpRetiraPagToStr(FCTe.ide.retira));
  AssertEquals('CTe.ide.xretira', EmptyStr, FCTe.ide.xDetRetira);
  AssertEquals('CTe.ide.dhCont', 0, FCTE.ide.dhCont);
  AssertEquals('CTe.ide.xJust', EmptyStr, FCTe.ide.xJust);
  AssertEquals('CTe.ide.IndIeToma', '1', indIEDestToStr(FCTe.ide.indIEToma));
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamentoOsDadosDoGrupo_toma3;
begin
  AssertEquals('CTe.ide.toma03', '4', TpTomadorToStr(FCTe.ide.toma03.Toma));
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_toma4;
begin
  AssertEquals('CTe.ide.toma4', '4', TpTomadorToStr(FCTe.ide.toma4.toma));
  AssertEquals('CTe.ide.toma4.CNPJCPF', '10242141000174', FCTe.ide.toma4.CNPJCPF);
  AssertEquals('CTe.ide.toma4.IE', '0010834420031', FCTe.ide.toma4.IE);
  AssertEquals('CTe.ide.toma4.xNome', 'ACOUGUE E SUPERMERCADO SOUZA LTDA', FCTe.ide.toma4.xNome);
  AssertEquals('CTe.ide.toma4.xFant', 'ACOUGUE E SUPERMERCADO SOUZA', FCTe.ide.toma4.xFant);
  AssertEquals('CTe.ide.toma4.fone', '11111111', FCTe.ide.toma4.fone);
  AssertEquals('CTe.ide.toma4.email', 'toma4@mail.com', FCTe.ide.toma4.email);

  //enderToma
  AssertEquals('CTe.ide.toma4.enderToma.xLgr', 'RUA BELO HORIZONTE', FCTe.ide.toma4.enderToma.xLgr);
  AssertEquals('CTe.ide.toma4.enderToma.nro', '614', FCTe.ide.toma4.enderToma.nro);
  AssertEquals('CTe.ide.toma4.enderToma.xCpl', 'N D', FCTe.ide.toma4.enderToma.xCpl);
  AssertEquals('CTe.ide.toma4.enderToma.xBairro', 'CALADINA', FCTe.ide.toma4.enderToma.xBairro);
  AssertEquals('CTe.ide.toma4.enderToma.cMun', 3119401, FCTe.ide.toma4.enderToma.cMun);
  AssertEquals('CTe.ide.toma4.enderToma.xMun', 'CORONEL FABRICIANO', FCTE.ide.toma4.enderToma.xMun);
  AssertEquals('CTe.ide.toma4.enderToma.CEP', 35171167, FCTe.ide.toma4.enderToma.CEP);
  AssertEquals('CTe.ide.toma4.enderToma.UF', 'MG', FCTe.ide.toma4.enderToma.UF);
  AssertEquals('CTe.ide.toma4.enderToma.cPais', 1058, FCTe.ide.toma4.enderToma.cPais);
  AssertEquals('CTe.ide.toma4.enderToma.xPais', 'BRASIL', FCTe.ide.toma4.enderToma.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_compl;
begin
  AssertEquals('CTe.compl.xCaracAd', 'Carac Adic', FCTe.compl.xCaracAd);
  AssertEquals('CTe.compl.xCaracSer', 'Carac Adicionais do Servico', FCTe.compl.xCaracSer);
  AssertEquals('CTe.compl.xEmi', 'Nome do Emitente', FCTe.compl.xEmi);
  AssertEquals('CTe.compl.origCalc', 'Sao Paulo', FCTe.compl.origCalc);
  AssertEquals('CTe.compl.destCalc', 'Campinas', FCTe.compl.destCalc);
  AssertEquals('CTe.compl.xObs', 'Observacao livre', FCTe.compl.xObs);

  //Fluxo
  AssertEquals('CTe.compl.fluxo.xOrig', EmptyStr, FCTe.compl.fluxo.xOrig);
  AssertEquals('CTe.compl.fluxo.xDest', EmptyStr, FCTe.compl.fluxo.xDest);
  AssertEquals('CTe.compl.fluxo.xRota', EmptyStr, FCTe.compl.fluxo.xRota);

  //pass
  AssertEquals('CTe.compl.fluxo.pass.Count', 0, FCTe.compl.fluxo.pass.Count);

  //Entrega
  AssertEquals('CTe.compl.Entrega.semData.tpPer', '0', TpDataPeriodoToStr(FCTe.compl.Entrega.semData.tpPer));
  AssertEquals('CTe.compl.Entrega.comData.tpPer', '0', TpDataPeriodoToStr(FCTe.compl.Entrega.comData.tpPer));
  AssertEquals('CTe.compl.Entrega.comData.dProg', 0, FCTe.compl.Entrega.comData.dProg);
  AssertEquals('CTe.compl.Entrega.noPeriodo.tpPer', '0', TpDataPeriodoToStr(FCTe.compl.Entrega.noPeriodo.tpPer));
  AssertEquals('CTe.compl.Entrega.noPeriodo.dIni', 0, FCTe.compl.Entrega.noPeriodo.dIni);
  AssertEquals('CTe.compl.Entrega.noPeriodo.dFim', 0, FCTe.compl.Entrega.noPeriodo.dFim);
  AssertEquals('CTe.compl.Entrega.semHora.tpHor', '0', TpHorarioIntervaloToStr(FCTe.compl.Entrega.semHora.tpHor));
  AssertEquals('CTe.compl.Entrega.comHora.tpHor', '0', TpHorarioIntervaloToStr(FCTe.compl.Entrega.comHora.tpHor));
  AssertEquals('CTe.compl.Entrega.comHora.hProg', 0, FCTe.compl.Entrega.comHora.hProg);
  AssertEquals('CTe.compl.Entrega.noInter.tpHor', '0', TpHorarioIntervaloToStr(FCTe.compl.Entrega.noInter.tpHor));
  AssertEquals('CTe.compl.Entrega.noInter.hIni', 0, FCTe.compl.Entrega.noInter.hIni);
  AssertEquals('CTe.compl.Entrega.noInter.hFim', 0, FCTe.compl.Entrega.noInter.hFim);

  //ObsCont
  AssertEquals('CTe.compl.ObsCont.Count', 2, FCTe.compl.ObsCont.Count);
  AssertEquals('CTe.compl.obsCont[0].xCampo', 'ObsCont01', FCTe.compl.ObsCont[0].xCampo);
  AssertEquals('CTe.compl.ObsCont[0].xTexto', 'Valor de ObsCont01', FCTe.compl.obsCont[0].xTexto);
  AssertEquals('CTe.compl.obsCont[1].xCampo', 'ObsCont02', FCTe.compl.ObsCont[1].xCampo);
  AssertEquals('CTe.compl.ObsCont[1].xTexto', 'Valor de ObsCont02', FCTe.compl.obsCont[1].xTexto);

  //ObsFisco
  AssertEquals('CTe.compl.ObsFisco.Count', 2, FCTe.compl.obsFisco.Count);
  AssertEquals('CTe.compl.obsFisco[0].xCampo', 'ObsFisco01', FCTe.compl.ObsFisco[0].xCampo);
  AssertEquals('CTe.compl.obsFisco[0].xTexto', 'Valor de ObsFisco01', FCTe.compl.ObsFisco[0].xTexto);
  AssertEquals('CTe.compl.obsFisco[1].xCampo', 'ObsFisco02', FCTe.compl.ObsFisco[1].xCampo);
  AssertEquals('CTe.compl.obsFisco[1].xTexto', 'Valor de ObsFisco02', FCTe.compl.ObsFisco[1].xTexto);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_emit;
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

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_rem;
begin
  AssertEquals('CTe.rem.CNPJCPF', '05481336000137', FCTe.rem.CNPJCPF);
  AssertEquals('CTe.rem.IE', '687138770110', FCTe.rem.IE);
  AssertEquals('CTe.rem.xNome', 'CTE EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL', FCTe.rem.xNome);
  AssertEquals('CTe.rem.xFant', 'Nome Fantasia', FCTe.rem.xFant);
  AssertEquals('CTe.rem.fone', '33445566', FCTe.rem.fone);
  AssertEquals('CTe.rem.email', 'rem@mail.com', FCTe.rem.email);

  //enderReme
  AssertEquals('CTe.rem.enderReme.xLgr', 'Rua 1', FCTe.rem.enderReme.xLgr);
  AssertEquals('CTe.rem.enderReme.nro', '200', FCTe.rem.enderReme.nro);
  AssertEquals('CTe.rem.enderReme.xCpl', 'xCPL REM', FCTe.rem.enderReme.xCpl);
  AssertEquals('CTe.rem.enderReme.xBairro', 'Centro', FCTe.rem.enderReme.xBairro);
  AssertEquals('CTe.rem.enderReme.cMun', 3554003, FCTe.rem.enderReme.cMun);
  AssertEquals('CTe.rem.enderReme.xMun', 'Nome do Municipio', FCTe.rem.enderReme.xMun);
  AssertEquals('CTe.rem.enderReme.CEP', 14123456, FCTe.rem.enderReme.CEP);
  AssertEquals('CTe.rem.enderReme.UF', 'SP', FCTe.rem.enderReme.UF);
  AssertEquals('CTe.rem.enderReme.cPais', 1058, FCTe.rem.enderReme.cPais);
  AssertEquals('CTe.rem.enderReme.xPais', 'BRASIL', FCTe.rem.enderReme.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_exped;
begin
  AssertEquals('CTe.exped.CNPJCPF', '12345678000123', FCTE.exped.CNPJCPF);
  AssertEquals('CTe.exped.IE', '12345678', FCTe.exped.IE);
  AssertEquals('CTe.exped.xNome', 'CTE EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL', FCTe.exped.xNome);
  AssertEquals('CTe.exped.fone', '33445566', FCTe.exped.fone);
  AssertEquals('CTe.exped.email', 'exped@mail.com', FCTe.exped.email);

  //enderExped
  AssertEquals('CTe.exped.xLgr', 'Rua 1', FCTe.exped.enderExped.xLgr);
  AssertEquals('CTe.exped.nro', '200', FCTe.exped.enderExped.nro);
  AssertEquals('CTe.exped.xCpl', 'xCpl Exped', FCTe.exped.enderExped.xCpl);
  AssertEquals('CTe.exped.xBairro', 'Centro', FCTe.exped.enderExped.xBairro);
  AssertEquals('CTe.exped.cMun', 3512345, FCTe.exped.enderExped.cMun);
  AssertEquals('CTe.exped.xMun', 'Nome do Municipio', FCTe.exped.enderExped.xMun);
  AssertEquals('CTe.exped.CEP', 14123456, FCTe.exped.enderExped.CEP);
  AssertEquals('CTe.exped.UF', 'SP', FCTe.exped.enderExped.UF);
  AssertEquals('CTe.exped.cPais', 1058, FCTe.exped.enderExped.cPais);
  AssertEquals('CTe.exped.xPais', 'BRASIL', FCTe.exped.enderExped.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_receb;
begin
  AssertEquals('CTe.receb.CNPJCPF', '12345678000123', FCTe.receb.CNPJCPF);
  AssertEquals('CTe.receb.IE', '12345678', FCTe.receb.IE);
  AssertEquals('CTe.receb.xNome', 'CTE EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL', FCTe.receb.xNome);
  AssertEquals('CTe.receb.fone', '33445566', FCTe.receb.fone);
  AssertEquals('CTe.receb.email', 'receb@mail.com', FCTe.receb.email);

  //enderReceb
  AssertEquals('CTe.receb.enderReceb.xLgr', 'Rua 1', FCTe.receb.enderReceb.xLgr);
  AssertEquals('CTe.receb.enderReceb.nro', '200', FCTe.receb.enderReceb.nro);
  AssertEquals('CTe.receb.enderReceb.xCpl', 'xCPL Receb', FCTe.receb.enderReceb.xCpl);
  AssertEquals('CTe.receb.enderReceb.xBairro', 'Centro', FCTe.receb.enderReceb.xBairro);
  AssertEquals('CTe.receb.enderReceb.cMun', 3512345, FCTe.receb.enderReceb.cMun);
  AssertEquals('CTe.receb.enderReceb.xMun', 'Nome do Municipio', FCTe.receb.enderReceb.xMun);
  AssertEquals('CTe.receb.enderReceb.CEP', 14123456, FCTe.receb.enderReceb.CEP);
  AssertEquals('CTe.receb.enderReceb.UF', 'SP', FCTe.receb.enderReceb.UF);
  AssertEquals('CTe.receb.enderReceb.cPais', 1058, FCTe.receb.enderReceb.cPais);
  AssertEquals('CTe.receb.enderReceb.xPais', 'BRASIL', FCTe.receb.enderReceb.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_dest;
begin
  AssertEquals('CTe.dest.CNPJ', '05481336000137', FCTe.dest.CNPJCPF);
  AssertEquals('CTe.dest.IE', '687138770110', FCTe.dest.IE);
  AssertEquals('CTe.dest.xNome', 'CTE EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL', FCTe.dest.xNome);
  AssertEquals('CTe.dest.fone', '33445566', FCTe.dest.fone);
  AssertEquals('CTe.dest.email', 'dest@mail.com', FCTe.dest.email);
  AssertEquals('CTe.dest.ISUF', EmptyStr, FCTe.dest.ISUF);

  //enderDest
  AssertEquals('CTe.dest.enderDest.xLgr', 'Rua 1', FCTe.dest.enderDest.xLgr);
  AssertEquals('CTe.dest.enderDest.nro', '200', FCTe.dest.enderDest.nro);
  AssertEquals('CTe.dest.enderDest.xCpl', 'xCPL Dest', FCTe.dest.enderDest.xCpl);
  AssertEquals('CTe.dest.enderDest.xBairro', 'Centro', FCTe.dest.enderDest.xBairro);
  AssertEquals('CTe.dest.enderDest.cMun', 3554003, FCTe.dest.enderDest.cMun);
  AssertEquals('CTe.dest.enderDest.xMun', 'Nome do Municipio', FCTe.dest.enderDest.xMun);
  AssertEquals('CTe.dest.enderDest.CEP', 14123456, FCTe.dest.enderDest.CEP);
  AssertEquals('CTe.dest.enderDest.UF', 'SP', FCTe.dest.enderDest.UF);
  AssertEquals('CTe.dest.enderDest.cPais', 1058, FCTe.dest.enderDest.cPais);
  AssertEquals('CTe.dest.enderDest.xPais', 'BRASIL', FCTe.dest.enderDest.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_vPrest;
begin
  AssertEquals('CTe.vPrest.vTPrest', 100, FCTe.vPrest.vTPrest);
  AssertEquals('CTe.vPrest.vRec', 100, FCTe.vPrest.vRec);

  //comp
  AssertEquals('CTe.vPrest.comp.Count', 2, FCTe.vPrest.Comp.Count);
  AssertEquals('CTe.vPrest.comp[0].xNome', 'DFRNER KRTJ', FCTe.vPrest.comp[0].xNome);
  AssertEquals('CTe.vPrest.comp[0].vComp', 100, FCTe.vPrest.comp[0].vComp);

  AssertEquals('CTe.vPrest.comp[0].xNome', 'DFRNER TJKR', FCTe.vPrest.comp[1].xNome);
  AssertEquals('CTe.vPrest.comp[0].vComp', 11.11, FCTe.vPrest.comp[1].vComp);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretemanteOsDadosDoGrupo_imp;
begin
  //ICMS90
  AssertEquals('CTe.imp.ICMS.ICMS90.CST', '90', CSTICMSToStr(FCTe.imp.ICMS.ICMS90.CST));
  AssertEquals('CTe.imp.ICMS.ICMS90.pRedBC', 10.00, FCTe.imp.ICMS.ICMS90.pRedBC);
  AssertEquals('CTe.imp.ICMS.ICMS90.vBC', 100.00, FCTe.imp.ICMS.ICMS90.vBC);
  AssertEquals('CTe.imp.ICMS.ICMS90.pICMS', 7.00, FCTe.imp.ICMS.ICMS90.pICMS);
  AssertEquals('CTe.imp.ICMS.ICMS90.vICMS', 7.00, FCTe.imp.ICMS.ICMS90.vICMS);

  AssertEquals('CTe.imp.vTotTrib', 17.00, FCTe.imp.vTotTrib);
  AssertEquals('CTe.imp.infAdFisco', 'Lei da Transparencia: O valor aproximado de tributos incidentes sobre o preco deste servico e de R$ 17,00 (17,00%) Fonte: IBPT', FCTe.imp.infAdFisco);

  AssertEquals('CTe.imp.ICMSUFFim.vBCUFFim', 100.00, FCTe.imp.ICMSUFFim.vBCUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.pFCPUFFim', 7.00, FCTe.imp.ICMSUFFim.pFCPUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.pICMSUFFim', 7.00, FCTe.imp.ICMSUFFim.pICMSUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.pICMSInter', 3.00, FCTe.imp.ICMSUFFim.pICMSInter);
  AssertEquals('CTe.imp.ICMSUFFim.vFCPUFFim', 4.00, FCTe.imp.ICMSUFFim.vFCPUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.vICMSUFFim', 7.00, FCTe.imp.ICMSUFFim.vICMSUFFim);
  AssertEquals('CTe.imp.ICMSUFFim.vICMSUFIni', 4.00, FCTe.imp.ICMSUFFim.vICMSUFIni);
end;


procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infCarga;
begin
  AssertEquals('CTe.infCteNorm.infCarga.vCarga', 5000, FCTe.infCTeNorm.infCarga.vCarga);
  AssertEquals('CTe.infCteNorm.infCarga.proPred', 'Produto Predominante', FCTe.infCTeNorm.infCarga.proPred);
  AssertEquals('CTe.infCteNorm.infCarga.xOutCat', 'Pacotes', FCTe.infCTeNorm.infCarga.xOutCat);
  AssertEquals('CTe.infCTeNorm.infCarga.vCargaAverb', 5000.00, FCTe.infCTeNorm.infCarga.vCargaAverb);

  //infQ
  AssertEquals('CTe.InfCTeNorm.infCarga.infQ.Count', 9, FCTe.infCTeNorm.infCarga.infQ.Count);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[0].cUnid', '01', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[0].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[0].tpMed', 'Kg', FCTe.infCTeNorm.infCarga.infQ[0].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[0].qCarga', 10.0000, FCTe.infCTeNorm.infCarga.infQ[0].qCarga);

  AssertEquals('CTe.infCTeNorm.infCarga.infQ[1].cUnid', '03', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[1].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[1].tpMed', 'Caixa', FCTe.infCTeNorm.infCarga.infQ[1].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[1].qCarga', 5.0000, FCTe.infCTeNorm.infCarga.infQ[1].qCarga);

  AssertEquals('CTe.infCTeNorm.infCarga.infQ[2].cUnid', '00', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[2].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[2].tpMed', 'Volume', FCTe.infCTeNorm.infCarga.infQ[2].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[2].qCarga', 10.0000, FCTe.infCTeNorm.infCarga.infQ[2].qCarga);

  AssertEquals('CTe.infCTeNorm.infCarga.infQ[3].cUnid', '02', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[3].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[3].tpMed', 'Toneladas', FCTe.infCTeNorm.infCarga.infQ[3].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[3].qCarga', 1.0000, FCTe.infCTeNorm.infCarga.infQ[3].qCarga);

  AssertEquals('CTe.infCTeNorm.infCarga.infQ[4].cUnid', '04', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[4].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[4].tpMed', 'Litros', FCTe.infCTeNorm.infCarga.infQ[4].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[4].qCarga', 10.0000, FCTe.infCTeNorm.infCarga.infQ[4].qCarga);

  AssertEquals('CTe.infCTeNorm.infCarga.infQ[5].cUnid', '04', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[5].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[5].tpMed', 'Litros2', FCTe.infCTeNorm.infCarga.infQ[5].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[5].qCarga', 10.0000, FCTe.infCTeNorm.infCarga.infQ[5].qCarga);

  AssertEquals('CTe.infCTeNorm.infCarga.infQ[6].cUnid', '04', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[6].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[6].tpMed', 'Litros3', FCTe.infCTeNorm.infCarga.infQ[6].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[6].qCarga', 10.0000, FCTe.infCTeNorm.infCarga.infQ[6].qCarga);

  AssertEquals('CTe.infCTeNorm.infCarga.infQ[7].cUnid', '04', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[7].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[7].tpMed', 'Litros4', FCTe.infCTeNorm.infCarga.infQ[7].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[7].qCarga', 10.0000, FCTe.infCTeNorm.infCarga.infQ[7].qCarga);

  AssertEquals('CTe.infCTeNorm.infCarga.infQ[8].cUnid', '04', UnidMedToStr(FCTe.infCTeNorm.infCarga.infQ[8].cUnid));
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[8].tpMed', 'Litros5', FCTe.infCTeNorm.infCarga.infQ[8].tpMed);
  AssertEquals('CTe.infCTeNorm.infCarga.infQ[8].qCarga', 10.0000, FCTe.infCTeNorm.infCarga.infQ[8].qCarga);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infNFe;
begin
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe.Count', 2, FCTe.infCTeNorm.infDoc.infNFe.Count);

  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].chave', '42210117089484000190550110000091001371413248', FCTe.infCTeNorm.infDoc.infNFe[0].chave);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].PIN', '98764321', FCTe.infCTeNorm.infDoc.infNFe[0].PIN);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[0].dPrev', EncodeDataHora('12/02/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.infDoc.infNFe[0].dPrev);

  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].chave', '42210117089484000190550110000091001371413248', FCTe.infCTeNorm.infDoc.infNFe[1].chave);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].PIN', '98754321' ,FCTe.infCTeNorm.infDoc.infNFe[1].PIN);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].dPrev', EncodeDataHora('12/02/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.infDoc.infNFe[1].dPrev);

  //infUnidTransp
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp.Count);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].tpUnidTransp', '7', UnidTranspToStr(FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].tpUnidTransp));
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].idUnidTransp', 'TP1', FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].idUnidTransp);

  //lacUnidTransp
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].lacUnidTransp.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].lacUnidTransp.Count);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].lacUnidTransp[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].lacUnidTransp[0].nLacre);

  //infUnidCarga
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga.Count);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].tpUnidCarga', '4', UnidCargaToStr(FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].tpUnidCarga));
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].idUnidCarga', 'T1', FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].idUnidCarga);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].qtdRat', 10.000, FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].qtdRat);

  //lacUnidCarga
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].lacUnidCarga.Count', 1, FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].lacUnidCarga.Count);
  AssertEquals('CTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].lacUnidCarga[0].nLacre', '1', FCTe.infCTeNorm.infDoc.infNFe[1].infUnidTransp[0].infUnidCarga[0].lacUnidCarga[0].nLacre);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_emiDocAnt;
begin
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt.Count', 1, FCTe.infCTeNorm.docAnt.emiDocAnt.Count);
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].CNPJCPF', '12345678910001', FCTe.infCTeNorm.docAnt.emiDocAnt[0].CNPJCPF);
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].IE', '12345678901234', FCTe.infCTeNorm.docAnt.emiDocAnt[0].IE);
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].UF', 'SP', FCTe.infCTeNorm.docAnt.emiDocAnt[0].UF);
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].xNome', 'Nome do Expedidor', FCTe.infCTeNorm.docAnt.emiDocAnt[0].xNome);

  //idDocAntEle
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt.Count', 1, FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt.Count);
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle.Count', 2, FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle.Count);
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle[0].chCte', '35240118760500000000570010000000011016595821', FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle[0].chCTe);
  AssertEquals('CTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle[1].chCte', '33240118760500000000570000000005051984519606', FCTe.infCTeNorm.docAnt.emiDocAnt[0].idDocAnt[0].idDocAntEle[1].chCTe);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_veicNovos;
begin
  AssertEquals('CTe.infCTeNorm.veicNovos.Count', 2, FCTe.infCTeNorm.veicNovos.Count);
  AssertEquals('CTe.infCTeNorm.veicNovos[0].chassi', '12345678901234567', FCTe.infCTeNorm.veicNovos[0].chassi);
  AssertEquals('CTe.infCTeNorm.veicNovos[0].cCor', '123', FCTe.infCTeNorm.veicNovos[0].cCor);
  AssertEquals('CTe.infCTeNorm.veicNovos[0].xCor', 'Cor1', FCTe.infCTeNorm.veicNovos[0].xCor);
  AssertEquals('CTe.infCTeNorm.veicNovos[0].cMod', '456', FCTe.infCTeNorm.veicNovos[0].cMod);
  AssertEquals('CTe.infCTeNorm.veicNovos[0].vUnit', 250000.00, FCTe.infCTeNorm.veicNovos[0].vUnit);
  AssertEquals('CTe.infCTeNorm.veicNovos[0].vFrete', 5000.00, FCTe.infCTeNorm.veicNovos[0].vFrete);

  AssertEquals('CTe.infCTeNorm.veicNovos[1].chassi', '98765432107654321', FCTe.infCTeNorm.veicNovos[1].chassi);
  AssertEquals('CTe.infCTeNorm.veicNovos[1].cCor', '321', FCTe.infCTeNorm.veicNovos[1].cCor);
  AssertEquals('CTe.infCTeNorm.veicNovos[1].xCor', 'Cor2', FCTe.infCTeNorm.veicNovos[1].xCor);
  AssertEquals('CTe.infCTeNorm.veicNovos[1].cMod', '654', FCTe.infCTeNorm.veicNovos[1].cMod);
  AssertEquals('CTe.infCTeNorm.veicNovos[1].vUnit', 260000.00, FCTe.infCTeNorm.veicNovos[1].vUnit);
  AssertEquals('CTe.infCTeNorm.veicNovos[1].vFrete', 6000.00, FCTe.infCTeNorm.veicNovos[1].vFrete);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_cobr;
begin
  AssertEquals('CTe.infCTeNorm.cobr.fat.nFat', '123', FCTe.infCTeNorm.cobr.fat.nFat);
  AssertEquals('CTe.infCTeNorm.cobr.fat.vOrig', 100.00, FCTe.infCTeNorm.cobr.fat.vOrig);
  AssertEquals('CTe.infCTeNorm.cobr.fat.vDesc', 0, FCTe.infCTeNorm.cobr.fat.vDesc);
  AssertEquals('CTe.infCTeNorm.cobr.fat.vLiq', 100.00, FCTe.infCTeNorm.cobr.fat.vLiq);

  //dup
  AssertEquals('CTe.infCTeNorm.cobr.dup.Count', 2, FCTe.infCTeNorm.cobr.dup.Count);
  AssertEquals('CTe.infCTeNorm.cobr.dup[0].nDup', '123', FCTe.infCTeNorm.cobr.dup[0].nDup);
  AssertEquals('CTe.infCTeNorm.cobr.dup[0].dVenc', EncodeDataHora('11/02/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.cobr.dup[0].dVenc);
  AssertEquals('CTe.infCTeNorm.cobr.dup[0].vDup', 100.00, FCTe.infCTeNorm.cobr.dup[0].vDup);
  AssertEquals('CTe.infCTeNorm.cobr.dup[1].nDup', '456', FCTe.infCTeNorm.cobr.dup[1].nDup);
  AssertEquals('CTe.infCTeNorm.cobr.dup[1].dVenc', EncodeDatahora('12/03/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.cobr.dup[1].dVenc);
  AssertEquals('CTe.infCTeNorm.cobr.dup[1].vDup', 100.00, FCTe.infCTeNorm.cobr.dup[1].vDup);

end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_autXML;
begin
  AssertEquals('CTe.autXML.Count', 2, FCTe.autXML.Count);
  AssertEquals('CTe.autXML[0].CNPJCPF', '99999999999999', FCTe.autXML[0].CNPJCPF);
  AssertEquals('CTe.autXML[1].CNPJCPF', '88888888888888', FCTe.autXML[1].CNPJCPF);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infRespTec;
begin
  AssertEquals('CTe.infRespTec.CNPJ', '12345678900001', FCTe.infRespTec.CNPJ);
  AssertEquals('CTe.infRespTec.xContato', 'xContatoRespTec', FCTe.infRespTec.xContato);
  AssertEquals('CTe.infRespTec.email', 'emailRespTec@mail.com', FCTe.infRespTec.email);
  AssertEquals('CTe.infRespTec.fone', '45455454', FCTe.infRespTec.fone);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infCTeSupl;
begin
  AssertEquals('CTe.infCTeSupl.qrCodCTe', 'https://homologacao.nfe.fazenda.sp.gov.br/CTeConsulta/qrCode?chCTe=35240118760500000000570010000000011121942170&tpAmb=2', FCTe.infCTeSupl.qrCodCTe);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_Signature;
begin
  AssertEquals('CTe.signature.DigestValue', 'SIiiX5+', FCTe.signature.DigestValue);
  AssertEquals('CTe.signature.SignatureValue', 'dyQlui8uKKBoL4T49M97+', FCTe.signature.SignatureValue);
  AssertEquals('CTe.signature.X509', 'MIIH5jCCB', FCTe.signature.X509Certificate);
  AssertEquals('CTe.signature.URI', '#CTe35240118760500000000570010000000011121942170', FCTe.signature.URI);
end;

procedure TACBrCTeXmlReaderReadingTests_CTe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_rodo;
begin
  AssertEquals('CTe.infCTeNorm.rodo.RNTRC', '12345678', FCTe.infCTeNorm.rodo.RNTRC);

  //occ
  AssertEquals('CTe.infCTeNorm.rodo.occ.Count', 1, FCTe.infCTeNorm.rodo.occ.Count);
  AssertEquals('CTe.infCTeNorm.rodo.occ[0].serie', '001', FCTe.infCTeNorm.rodo.occ[0].serie);
  AssertEquals('CTe.infCTeNorm.rodo.occ[0].nOcc', 1, FCTe.infCTeNorm.rodo.occ[0].nOcc);
  AssertEquals('CTe.infCTeNorm.rodo.occ[0].dEmi', EncodeDataHora('12/01/2024', 'DD/MM/YYYY'), FCTe.infCTeNorm.rodo.occ[0].dEmi);
  AssertEquals('CTe.infCTeNorm.rodo.occ[0].CNPJ', '12345678000123', FCTe.infCTeNorm.rodo.occ[0].emiOcc.CNPJ);
  AssertEquals('CTe.infCTeNorm.rodo.occ[0].cInt', '501', FCTe.infCTeNorm.rodo.occ[0].emiOcc.cInt);
  AssertEquals('CTe.infCTeNorm.rodo.occ[0].IE', '1234567', FCTe.infCTeNorm.rodo.occ[0].emiOcc.IE);
  AssertEquals('CTe.infCTeNorm.rodo.occ[0].UF', 'SP', FCTe.infCTeNorm.rodo.occ[0].emiOcc.UF);
  AssertEquals('CTe.infCTeNorm.rodo.occ[0].fone', '22334455', FCTe.infCTeNorm.rodo.occ[0].emiOcc.fone);
end;

initialization
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.XMLCompleto', TACBrCTeXmlReaderReadingTests_CTe_Ver400);
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.Complementar', TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver400);
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.ChoiceGroups', TACBrCTeXmlReaderReadingTests_GruposModais_Ver400);
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.ChoiceGroups', TACBrCTeXmlReaderReadingTests_GruposICMS_ver400);
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.ChoiceGroups', TACBrCTeXmlReaderReadingTests_GrupoInfDoc_ver400);
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.ChoiceGroups', TACBrCTeXmlReaderReadingTests_GrupoIdDocAnt_ver400);

end.

