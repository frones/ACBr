unit ACBrCTeXmlReaderReadingTests_GTVe_Ver400;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ACBrCTeXmlHandler, ACBrTests.Util, pcteCTe;

type

  { TACBrCTeXmlReaderReadingTests_GTVe_Ver400 }

  TACBrCTeXmlReaderReadingTests_GTVe_Ver400 = class(TTestCase)
  private
    FCTe: TCTe;
    FCTeReader: TCTeXmlReader;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LerXML_LeuCorretamenteOsAtributosDoGrupo_InfCTe;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_ide;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_toma;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_tomaTerceiro;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_compl;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_emit;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_rem;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_dest;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_origem;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_destino;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_detGTV;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_autXML;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infRespTec;
    procedure LerXML_LeuCorretamenteOsDadosDoGrupo_InfCTeSupl;
  end;

implementation

uses
  ACBrCTeTestConsts, ACBrUtil.DateTime, pcnConversao, pcteConversaoCTe;

{ TACBrCTeXmlReaderReadingTests_GTVe_Ver400 }

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.SetUp;
begin
  inherited SetUp;
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  FCTeReader.CarregarArquivo(XML_GTVe_NORMAL_VERSAO400);
  FCTeReader.LerXML;
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.TearDown;
begin
  inherited TearDown;
  FCTeReader.Free;
  FCTe.Free;
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsAtributosDoGrupo_InfCTe;
begin
  AssertEquals('CTe.InfCTe.Versao', 4, FCTe.infCTe.versao);
  AssertEquals('CTe.infCTe.Id', 'CTe35240118760500000000640010000000011773821899', FCTe.infCTe.Id);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_ide;
begin
  AssertEquals('CTe.ide.cUF', 35, FCTe.ide.cUF);
  AssertEquals('CTe.ide.cCT', 77382189, FCTe.ide.cCT);
  AssertEquals('CTe.ide.CFOP', 5353, FCTe.ide.CFOP);
  AssertEquals('CTe.ide.natOP', 'PRESTACAO SERVICO', FCTe.ide.natOp);
  AssertEquals('CTe.ide.mod', 64, FCTe.ide.modelo);
  AssertEquals('CTe.ide.serie', 1, FCTe.ide.serie);
  AssertEquals('CTe.ide.nCT', 1, FCTe.ide.nCT);
  AssertEquals('CTe.ide.dhEmi', EncodeDataHora('22/01/2024 11:24:53', 'dd/mm/yyyy hh:nn:ss'), FCTe.ide.dhEmi);
  AssertEquals('CTe.ide.tpImp', '1', TpImpToStr(FCTe.ide.tpImp));
  AssertEquals('CTe.ide.tpEmis', '1', TpEmisToStr(FCTe.ide.tpEmis));
  AssertEquals('CTe.ide.cDV', 9, FCTe.ide.cDV);
  AssertEquals('CTe.ide.tpAmb', '2', TpAmbToStr(FCTe.ide.tpAmb));
  AssertEquals('CTe.ide.tpCTe', '4', tpCTePagToStr(FCTe.ide.tpCte));
  AssertEquals('CTe.ide.verProc', '3.0', FCTe.ide.verProc);
  AssertEquals('CTe.ide.cMunEnv', 3554003, FCTe.ide.cMunEnv);
  AssertEquals('CTe.ide.xMunEnv', 'TATUI', FCTe.ide.xMunEnv);
  AssertEquals('CTe.ide.UFEnv', 'SP', FCTe.ide.UFEnv);
  AssertEquals('CTe.ide.modal', '01', TpModalToStr(FCTe.ide.modal));
  AssertEquals('CTe.ide.tpServ', '9', TpServPagToStr(FCTe.ide.tpServ));
  AssertEquals('CTe.ide.IndIeToma', '1', indIEDestToStr(FCTe.ide.indIEToma));
  AssertEquals('CTe.ide.dhSaidaOrig', EncodeDataHora('22/01/2024 11:24:53', 'DD/MM/YYYY hh:nn:ss'), FCTe.ide.dhSaidaOrig);
  AssertEquals('CTe.ide.dhChegadaDest', EncodeDataHora('23/01/2024 11:24:53', 'DD/MM/YYYY hh:nn:ss'), FCTe.ide.dhChegadaDest);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_toma;
begin
  AssertEquals('CTe.ide.toma', '4', TpTomadorToStr(FCTe.ide.toma03.Toma));
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_tomaTerceiro;
begin
  AssertEquals('CTe.ide.tomaTerceiro.toma', '4', TpTomadorToStr(FCTe.ide.toma4.toma));
  AssertEquals('CTe.ide.tomaTerceiro.CNPJCPF', '11111111111112', FCTe.ide.toma4.CNPJCPF);
  AssertEquals('CTe.ide.tomaTerceiro.IE', '123456789012345', FCTe.ide.toma4.IE);
  AssertEquals('CTe.ide.tomaTerceiro.xNome', 'XNomeTomaTerceiro', FCTe.ide.toma4.xNome);
  AssertEquals('CTe.ide.tomaTerceiro.xFant', 'XFantTomaTerceiro', FCTe.ide.toma4.xFant);
  AssertEquals('CTe.ide.tomaTerceiro.fone', '11111111', FCTe.ide.toma4.fone);
  AssertEquals('CTe.ide.tomaTerceiro.email', 'tomaterceiro@mail.com', FCTe.ide.toma4.email);

  //enderToma
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.xLgr', 'XLgr', FCTe.ide.toma4.enderToma.xLgr);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.nro', '1', FCTe.ide.toma4.enderToma.nro);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.xCpl', 'xCpl', FCTe.ide.toma4.enderToma.xCpl);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.xBairro', 'xBairro', FCTe.ide.toma4.enderToma.xBairro);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.cMun', 5201108, FCTe.ide.toma4.enderToma.cMun);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.xMun', 'Anapolis', FCTe.ide.toma4.enderToma.xMun);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.CEP', 11111111, FCTe.ide.toma4.enderToma.CEP);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.UF', 'GO', FCTe.ide.toma4.enderToma.UF);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.cPais', 1058, FCTe.ide.toma4.enderToma.cPais);
  AssertEquals('CTe.ide.tomaTerceiro.enderToma.xPais', 'Brasil', FCTe.ide.toma4.enderToma.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_compl;
begin
  AssertEquals('CTe.compl.xCaracAd', 'Carac. Adic. Tr', FCTe.compl.xCaracAd);
  AssertEquals('CTe.compl.xCaracSer', 'Carac. Adic. do Servico', FCTe.compl.xCaracSer);
  AssertEquals('CTe.compl.xEmi', 'Nome do Emitente', FCTe.compl.xEmi);
  AssertEquals('CTe.compl.xObs', 'Observacao livre', FCTe.compl.xObs);

  //ObsCont
  AssertEquals('CTe.compl.ObsCont.Count', 1, FCTe.compl.ObsCont.Count);
  AssertEquals('CTe.compl.obsCont[0].xCampo', 'Nome do Campo', FCTe.compl.ObsCont[0].xCampo);
  AssertEquals('CTe.compl.ObsCont[0].xTexto', 'Valor do Campo', FCTe.compl.obsCont[0].xTexto);

  //ObsFisco
  AssertEquals('CTe.compl.ObsFisco.Count', 1, FCTe.compl.obsFisco.Count);
  AssertEquals('CTe.compl.obsFisco[0].xCampo', 'Nome do Campo', FCTe.compl.ObsFisco[0].xCampo);
  AssertEquals('CTe.compl.obsFisco[0].xTexto', 'Valor do Campo', FCTe.compl.ObsFisco[0].xTexto);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_emit;
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

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_rem;
begin
  AssertEquals('CTe.rem.CNPJCPF', '05481336000137', FCTe.rem.CNPJCPF);
  AssertEquals('CTe.rem.IE', '12345678', FCTe.rem.IE);
  AssertEquals('CTe.rem.xNome', 'CTE EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL', FCTe.rem.xNome);
  AssertEquals('CTe.rem.xFant', 'Nome Fantasia', FCTe.rem.xFant);
  AssertEquals('CTe.rem.fone', '33445566', FCTe.rem.fone);
  AssertEquals('CTe.rem.email', 'rem@mail.com', FCTe.rem.email);

  //enderReme
  AssertEquals('CTe.rem.enderReme.xLgr', 'Rua 1', FCTe.rem.enderReme.xLgr);
  AssertEquals('CTe.rem.enderReme.nro', '200', FCTe.rem.enderReme.nro);
  AssertEquals('CTe.rem.enderReme.xCpl', 'RemxCpl', FCTe.rem.enderReme.xCpl);
  AssertEquals('CTe.rem.enderReme.xBairro', 'Centro', FCTe.rem.enderReme.xBairro);
  AssertEquals('CTe.rem.enderReme.cMun', 3554003, FCTe.rem.enderReme.cMun);
  AssertEquals('CTe.rem.enderReme.xMun', 'Nome do Municipio', FCTe.rem.enderReme.xMun);
  AssertEquals('CTe.rem.enderReme.CEP', 14123456, FCTe.rem.enderReme.CEP);
  AssertEquals('CTe.rem.enderReme.UF', 'SP', FCTe.rem.enderReme.UF);
  AssertEquals('CTe.rem.enderReme.cPais', 1058, FCTe.rem.enderReme.cPais);
  AssertEquals('CTe.rem.enderReme.xPais', 'BRASIL', FCTe.rem.enderReme.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_dest;
begin
  AssertEquals('CTe.dest.CNPJ', '05481336000137', FCTe.dest.CNPJCPF);
  AssertEquals('CTe.dest.IE', '12345678', FCTe.dest.IE);
  AssertEquals('CTe.dest.xNome', 'CTE EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL', FCTe.dest.xNome);
  AssertEquals('CTe.dest.fone', '33445566', FCTe.dest.fone);
  AssertEquals('CTe.dest.email', 'dest@mail.com', FCTe.dest.email);
  AssertEquals('CTe.dest.ISUF', EmptyStr, FCTe.dest.ISUF);

  //enderDest
  AssertEquals('CTe.dest.enderDest.xLgr', 'Rua 1', FCTe.dest.enderDest.xLgr);
  AssertEquals('CTe.dest.enderDest.nro', '200', FCTe.dest.enderDest.nro);
  AssertEquals('CTe.dest.enderDest.xCpl', 'DestxCpl', FCTe.dest.enderDest.xCpl);
  AssertEquals('CTe.dest.enderDest.xBairro', 'Centro', FCTe.dest.enderDest.xBairro);
  AssertEquals('CTe.dest.enderDest.cMun', 3554003, FCTe.dest.enderDest.cMun);
  AssertEquals('CTe.dest.enderDest.xMun', 'Nome do Municipio', FCTe.dest.enderDest.xMun);
  AssertEquals('CTe.dest.enderDest.CEP', 14123456, FCTe.dest.enderDest.CEP);
  AssertEquals('CTe.dest.enderDest.UF', 'SP', FCTe.dest.enderDest.UF);
  AssertEquals('CTe.dest.enderDest.cPais', 1058, FCTe.dest.enderDest.cPais);
  AssertEquals('CTe.dest.enderDest.xPais', 'BRASIL', FCTe.dest.enderDest.xPais);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_origem;
begin
  AssertEquals('CTe.origem.xLgr', 'origemxLgr', FCTe.origem.xLgr);
  AssertEquals('CTe.origem.nro', 'O-123', FCTe.origem.nro);
  AssertEquals('CTe.origem.xCpl', 'origemxCpl', FCTe.origem.xCpl);
  AssertEquals('CTe.origem.xBairro', 'origemxBairro', FCTe.origem.xBairro);
  AssertEquals('CTe.origem.cMun', 3554003, FCTe.origem.cMun);
  AssertEquals('CTe.origem.xMun', 'Tatui', FCTe.origem.xMun);
  AssertEquals('CTe.origem.CEP', 12345678, FCTe.origem.CEP);
  AssertEquals('CTe.origem.UF', 'SP', FCTe.origem.UF);
  AssertEquals('CTe.origem.fone', '22223333', FCTe.origem.fone);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_destino;
begin
  AssertEquals('CTe.destino.xLgr', 'destinoxLgr', FCTe.destino.xLgr);
  AssertEquals('CTe.destino.nro', 'D-123', FCTe.destino.nro);
  AssertEquals('CTe.destino.xCpl', 'destinoxCpl', FCTe.destino.xCpl);
  AssertEquals('CTe.destino.xBairro', 'destinoxBairro', FCTe.destino.xBairro);
  AssertEquals('CTe.destino.cMun', 3554102, FCTe.destino.cMun);
  AssertEquals('CTe.destino.xMun', 'Taubate', FCTe.destino.xMun);
  AssertEquals('CTe.destino.CEP', 12345678, FCTe.destino.CEP);
  AssertEquals('CTe.destino.UF', 'SP', FCTe.destino.UF);
  AssertEquals('CTe.destino.fone', '33332222', FCTe.destino.fone);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_detGTV;
begin
  AssertEquals('CTe.detGTV.infEspecie.Count', 1, FCTe.detGTV.infEspecie.Count);
  AssertEquals('CTe.detGTV.infEspecie[0].tpEspecie', '1', TEspecieToStr(FCTe.detGTV.infEspecie[0].tpEspecie));
  AssertEquals('CTe.detGTV.infEspecie[0].vEspecie', 5000.00, FCTe.detGTV.infEspecie[0].vEspecie);
  AssertEquals('CTe.detGTV.infEspecie[0].tpNumerario', '1', tpNumerarioToStr(FCTe.detGTV.infEspecie[0].tpNumerario));
  AssertEquals('CTe.detGTV.infEspecie[0].xMoedaEstr', 'Nacional', FCTe.detGTV.infEspecie[0].xMoedaEstr);
  AssertEquals('CTe.detGTV.qCarga', 1.0000, FCTe.detGTV.qCarga);

  AssertEquals('CTe.detGTV.infVeiculo.Count', 1, FCTe.detGTV.infVeiculo.Count);
  AssertEquals('CTe.detGTV.infVeiculo[0].placa', 'XYZ1234', FCTe.detGTV.infVeiculo[0].placa);
  AssertEquals('CTe.detGTV.infVeiculo[0].UF', 'SP', FCTe.detGTV.infVeiculo[0].UF);
  AssertEquals('CTe.detGTV.infVeiculo[0].RNTRC', '1234A', FCTe.detGTV.infVeiculo[0].RNTRC);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_autXML;
begin
  AssertEquals('CTe.autXML.Count', 1, FCte.autXML.Count);
  AssertEquals('CTe.autXML[0].CNPJCPF', '99999999999999', FCTe.autXml[0].CNPJCPF);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_infRespTec;
begin
  AssertEquals('CTe.infRespTec.CNPJ', '999999999888888', FCTe.infRespTec.CNPJ);
  AssertEquals('CTe.infRespTec.xContato', 'infRespTecXContato', FCTe.infRespTec.xContato);
  AssertEquals('CTe.infRespTec.email', 'infRespTecemail', FCTe.infRespTec.email);
  AssertEquals('CTe.infRespTec.fone', 'infRespTecFone', FCTe.infRespTec.fone);
end;

procedure TACBrCTeXmlReaderReadingTests_GTVe_Ver400.LerXML_LeuCorretamenteOsDadosDoGrupo_InfCTeSupl;
begin
  AssertEquals('CTe.infCTeSupl.qrCodCTe', 'https://homologacao.nfe.fazenda.sp.gov.br/CTeConsulta/qrCode?chCTe=35240118760500000000640010000000011773821899&tpAmb=2', FCTe.infCTeSupl.qrCodCTe);
end;

initialization
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.XMLCompleto', TACBrCTeXmlReaderReadingTests_GTVe_Ver400);

end.

