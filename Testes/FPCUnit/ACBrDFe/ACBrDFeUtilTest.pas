unit ACBrDFeUtilTest;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrTests.Util;

type

  { FormatarNumeroDocumentoFiscalTest }
  FormatarNumeroDocumentoFiscalTest = class(TTestCase)
  published
    procedure Formata999999999;
    procedure Formata000000009;
    procedure Formata000009000;
    procedure Formata009000000;
    procedure Formata009009009;
    procedure Formata1;
    procedure Formata1000;
    procedure Formata1000000;
    procedure Formata100000000;
  end;

  FormatarChaveAcessoTest = class(TTestCase)
  published
    procedure FormataChave;
  end;

  { FormatarNumeroDocumentoFiscalNFSeTest }
  FormatarNumeroDocumentoFiscalNFSeTest = class(TTestCase)
  published
    procedure Formata999999999999999;
    procedure Formata000000000000009;
    procedure Formata000900000000000;
    procedure Formata000090000000000;
    procedure Formata1;
    procedure Formata10000000000;
    procedure Formata100000000000;
    procedure Formata100000000000000;
  end;

  ValidaUFCidadeTest = class(TTestCase)
  published
    procedure UFIgual_RetornaTrue;
    procedure UFDiferente_RetornaFalso;
    procedure UFDiferente_Com_Msg_LevantaExcecao;
  end;

  ValidaDIDSITest = class(TTestCase)
  published
    procedure Maior_que_12_Caracteres_RetornaFalso;
    procedure Menor_que_11_Caracteres_RetornaFalso;
    procedure Tipo_Diferente_de_2_e_4_RetornaFalso;
    procedure Digito_Incorreto_RetornaFalso;
    procedure Tipo_2_Valido_RetornaTrue;
    procedure Tipo_4_Valido_RetornaTrue;
    procedure Ano_Incorreto_RetornaFalso;
    procedure ComLetras_RetornaFalso;
  end;

  ValidaDIRETest = class(TTestCase)
  published
    procedure Diferente_de_12_Caracteres_RetornaFalso;
    procedure Ano_Incorreto_RetornaFalso;
    procedure ComLetras_RetornaFalso;
    procedure DIRE_Valida_RetornaTrue;
  end;

  ValidaRETest = class(TTestCase)
  published
    procedure Ano_Incorreto_RetornaFalso;
    procedure Diferente_de_12_Caracteres_RetornaFalso;
    procedure Serie_Incorreta_RetornaFalso;
    procedure RE_Valida_RetornaTrue;
    procedure ComLetras_RetornaFalso;
  end;

  ValidaDrawbackTest = class(TTestCase)
  published
    procedure Ano_Incorreto_RetornaFalso;
    procedure Diferente_de_11_Caracteres_RetornaFalso;
    procedure Digito_Incorreto_RetornaFalso;
    procedure ComLetras_RetornaFalso;
    procedure Drawback_Valido_RetornaTrue;
  end;

  ValidaRECOPITest = class(TTestCase)
  published
    procedure Diferente_de_20_Caracteres_RetornaFalso;
    procedure Digito_Incorreto_RetornaFalso;
    procedure ComLetras_RetornaFalso;
    procedure RECOPI_Valido_RetornaTrue;
  end;

  EncontrarURITest = class(TTestCase)
  published
    procedure Nao_Possui_ID_RetornaVazio;
    procedure Nao_Possui_Aspa_Inicial_LevantaExcecao;
    procedure Nao_Possui_Aspa_Final_LevantaExcecao;
    procedure URI_Correta_Retorna_URI;
  end;

  XmlEstaAssinadoTest = class(TTestCase)
  published
    procedure XML_Assinado_CaixaAlta_RetornaTrue;
    procedure XML_Assinado_CaixaBaixa_RetornaTrue;
    procedure XML_Nao_Assinado_RetornaFalso;
  end;

  URLServicosTest = class(TTestCase)
  private
    NomeArquivo: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckURLNFCe;
  end;

implementation

uses
  Math, dateutils, IniFiles,
  pcnConversao,
  ACBrUtil, ACBrDFeUtil, ACBrConsts, ACBrValidador, ACBrDFeException, ACBrNFe;

{ FormatarNumeroDocumentoFiscalTest }

procedure FormatarNumeroDocumentoFiscalTest.Formata000000009;
begin
  CheckEquals('000.000.009', FormatarNumeroDocumentoFiscal('000000009'));
end;

procedure FormatarNumeroDocumentoFiscalTest.Formata000009000;
begin
  CheckEquals('000.009.000', FormatarNumeroDocumentoFiscal('000009000'));
end;

procedure FormatarNumeroDocumentoFiscalTest.Formata009000000;
begin
  CheckEquals('009.000.000', FormatarNumeroDocumentoFiscal('009000000'));
end;

procedure FormatarNumeroDocumentoFiscalTest.Formata009009009;
begin
  CheckEquals('009.009.009', FormatarNumeroDocumentoFiscal('009009009'));
end;

procedure FormatarNumeroDocumentoFiscalTest.Formata1;
begin
  CheckEquals('000.000.001', FormatarNumeroDocumentoFiscal('1'));
end;

procedure FormatarNumeroDocumentoFiscalTest.Formata1000;
begin
  CheckEquals('000.001.000', FormatarNumeroDocumentoFiscal('1000'));
end;

procedure FormatarNumeroDocumentoFiscalTest.Formata1000000;
begin
  CheckEquals('001.000.000', FormatarNumeroDocumentoFiscal('1000000'));
end;

procedure FormatarNumeroDocumentoFiscalTest.Formata100000000;
begin
  CheckEquals('100.000.000', FormatarNumeroDocumentoFiscal('100000000'));
end;

procedure FormatarNumeroDocumentoFiscalTest.Formata999999999;
begin
  CheckEquals('999.999.999', FormatarNumeroDocumentoFiscal('999999999'));
end;

{ FormatarNumeroDocumentoFiscalNFSeTest }

procedure FormatarNumeroDocumentoFiscalNFSeTest.Formata000000000000009;
begin
  CheckEquals('0000.00000000009', FormatarNumeroDocumentoFiscalNFSe('000000000000009'));
end;

procedure FormatarNumeroDocumentoFiscalNFSeTest.Formata000090000000000;
begin
  CheckEquals('0000.90000000000', FormatarNumeroDocumentoFiscalNFSe('000090000000000'));
end;

procedure FormatarNumeroDocumentoFiscalNFSeTest.Formata000900000000000;
begin
  CheckEquals('0009.00000000000', FormatarNumeroDocumentoFiscalNFSe('000900000000000'));
end;

procedure FormatarNumeroDocumentoFiscalNFSeTest.Formata1;
begin
  CheckEquals('0000.00000000001', FormatarNumeroDocumentoFiscalNFSe('1'));
end;

procedure FormatarNumeroDocumentoFiscalNFSeTest.Formata10000000000;
begin
  CheckEquals('0000.10000000000', FormatarNumeroDocumentoFiscalNFSe('10000000000'));
end;

procedure FormatarNumeroDocumentoFiscalNFSeTest.Formata100000000000;
begin
  CheckEquals('0001.00000000000', FormatarNumeroDocumentoFiscalNFSe('100000000000'));
end;

procedure FormatarNumeroDocumentoFiscalNFSeTest.Formata100000000000000;
begin
  CheckEquals('1000.00000000000', FormatarNumeroDocumentoFiscalNFSe('100000000000000'));
end;

procedure FormatarNumeroDocumentoFiscalNFSeTest.Formata999999999999999;
begin
  CheckEquals('9999.99999999999', FormatarNumeroDocumentoFiscalNFSe('999999999999999'));
end;

{ ValidaUFCidadeTest }

procedure ValidaUFCidadeTest.UFDiferente_RetornaFalso;
begin
  CheckFalse(ValidaUFCidade(32, 3304144));
end;

procedure ValidaUFCidadeTest.UFDiferente_Com_Msg_LevantaExcecao;
const
  MSG = 'Codigo da UF e da Cidade estão errados';
begin
  try
    ValidaUFCidade(32, 3304144, MSG);
  except on e: Exception do
    begin
      Check(e is EACBrDFeException);
      CheckEquals(ACBrStr(MSG), e.Message);
    end
  end;
end;

procedure ValidaUFCidadeTest.UFIgual_RetornaTrue;
begin
  CheckTrue(ValidaUFCidade(33, 3304144));
end;

{ FormatarChaveAcessoTest }

procedure FormatarChaveAcessoTest.FormataChave;
begin
  CheckEquals('9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999', FormatarChaveAcesso('99999999999999999999999999999999999999999999'));
end;

{ ValidaDIDSITest }

procedure ValidaDIDSITest.Tipo_Diferente_de_2_e_4_RetornaFalso;
var
  Ano, DIDSI, Digito: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIDSI := '3' + Ano + '0000001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  CheckFalse(ValidaDIDSI(DIDSI));
end;

procedure ValidaDIDSITest.Maior_que_12_Caracteres_RetornaFalso;
var
  Ano, DIDSI, Digito: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIDSI := '2' + Ano + '000000001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  CheckFalse(ValidaDIDSI(DIDSI));
end;

procedure ValidaDIDSITest.Menor_que_11_Caracteres_RetornaFalso;
var
  Ano, DIDSI, Digito: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIDSI := '2' + Ano + '000001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  CheckFalse(ValidaDIDSI(DIDSI));
end;

procedure ValidaDIDSITest.Digito_Incorreto_RetornaFalso;
var
  Ano, DIDSI, Digito: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIDSI := '2' + Ano + '0000001';
  Digito := Modulo11(DIDSI);
  if Digito = '0' then
    Digito := '1'
  else Digito := '0';
  DIDSI := DIDSI + Digito;
  CheckFalse(ValidaDIDSI(DIDSI));
end;

procedure ValidaDIDSITest.Tipo_2_Valido_RetornaTrue;
var
  Ano, DIDSI, Digito: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIDSI := '2' + Ano + '0000001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  Check(ValidaDIDSI(DIDSI));
end;

procedure ValidaDIDSITest.Tipo_4_Valido_RetornaTrue;
var
  Ano, DIDSI, Digito: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIDSI := '4' + Ano + '0000001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  Check(ValidaDIDSI(DIDSI));
end;

procedure ValidaDIDSITest.Ano_Incorreto_RetornaFalso;
var
  Ano, DIDSI, Digito: string;
begin
  Ano := FormatDateTime('YY', EncodeDate(YearOf(Date)+2, 1, 1));
  DIDSI := '4' + Ano + '0000001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  CheckFalse(ValidaDIDSI(DIDSI), 'Ano superior deveria ser inválido');

  Ano := FormatDateTime('YY', EncodeDate(YearOf(Date)-2, 1, 1));
  DIDSI := '4' + Ano + '0000001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  CheckFalse(ValidaDIDSI(DIDSI), 'Ano inferior deveria ser inválido');
end;

procedure ValidaDIDSITest.ComLetras_RetornaFalso;
var
  Ano, DIDSI, Digito: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIDSI := '4' + Ano + 'A000001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  CheckFalse(ValidaDIDSI(DIDSI));

  DIDSI := '2' + Ano + '000B001';
  Digito := Modulo11(DIDSI);
  DIDSI := DIDSI + Digito;
  CheckFalse(ValidaDIDSI(DIDSI));
end;

{ ValidaDIRETest }

procedure ValidaDIRETest.Ano_Incorreto_RetornaFalso;
var
  Ano, DIRE: string;
begin
  Ano := FormatDateTime('YY', EncodeDate(YearOf(Date)+2, 1, 1));
  DIRE := Ano + '0000000001';
  CheckFalse(ValidaDIRE(DIRE), 'Ano superior deveria ser inválido');

  Ano := FormatDateTime('YY', EncodeDate(YearOf(Date)-2, 1, 1));
  DIRE := Ano + '0000000001';
  CheckFalse(ValidaDIRE(DIRE), 'Ano inferior deveria ser inválido');
end;

procedure ValidaDIRETest.ComLetras_RetornaFalso;
var
  Ano, DIRE: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIRE := Ano + 'A000000001';
  CheckFalse(ValidaDIRE(DIRE));

  DIRE := Ano + '00000B0001';
  CheckFalse(ValidaDIRE(DIRE));

  DIRE := Ano + 'A0000000001';
  CheckFalse(ValidaDIRE(DIRE));
end;

procedure ValidaDIRETest.Diferente_de_12_Caracteres_RetornaFalso;
const
  ONZE_CARACTERES = '00000000001';
  NOVE_CARACTERES = '000000001';
var
  Ano, DIRE: string;
begin
  Ano := FormatDateTime('YY', Date);

  DIRE := Ano + ONZE_CARACTERES;
  CheckFalse(ValidaDIRE(DIRE), 'DIRE com mais de 12 caracteres deveria ser inválido');

  DIRE := Ano + NOVE_CARACTERES;
  CheckFalse(ValidaDIRE(DIRE), 'DIRE com menos de 12 caracteres deveria ser inválido');
end;

procedure ValidaDIRETest.DIRE_Valida_RetornaTrue;
var
  Ano, DIRE: string;
begin
  Ano := FormatDateTime('YY', Date);
  DIRE := Ano + '0000000001';
  Check(ValidaDIRE(DIRE));

  DIRE := Ano + '9999999999';
  Check(ValidaDIRE(DIRE));
end;

{ ValidaRETest }

procedure ValidaRETest.Ano_Incorreto_RetornaFalso;
var
  Ano, RE: string;
begin
  Ano := FormatDateTime('YY', EncodeDate(YearOf(Date)+2, 1, 1));
  RE := Ano + '0000001'+ '001';
  CheckFalse(ValidaRE(RE), 'Ano superior deveria ser inválido');

  Ano := FormatDateTime('YY', EncodeDate(YearOf(Date)-2, 1, 1));
  RE := Ano + '0000001'+ '001';
  CheckFalse(ValidaRE(RE), 'Ano inferior deveria ser inválido');
end;

procedure ValidaRETest.ComLetras_RetornaFalso;
var
  Ano, RE: string;
begin
  Ano := FormatDateTime('YY', Date);
  RE := Ano + 'A000001'+ '001';
  CheckFalse(ValidaRE(RE));

  RE := Ano + 'A0000001'+ '001';
  CheckFalse(ValidaRE(RE));
end;

procedure ValidaRETest.Diferente_de_12_Caracteres_RetornaFalso;
const
  OITO_CARACTERES = '00000001';
  SEIS_CARACTERES = '000001';
var
  Ano, RE: string;
begin
  Ano := FormatDateTime('YY', Date);
  RE := Ano + OITO_CARACTERES +'001';
  CheckFalse(ValidaRE(RE), 'RE com mais de 12 caracteres deveria ser inválido');

  Ano := FormatDateTime('YY', Date);
  RE := Ano + SEIS_CARACTERES +'001';
  CheckFalse(ValidaRE(RE), 'RE com menos de 12 caracteres deveria ser inválido');
end;

procedure ValidaRETest.RE_Valida_RetornaTrue;
var
  Ano, RE: string;
begin
  Ano := FormatDateTime('YY', Date);
  RE := Ano + '0000001'+ '001';
  Check(ValidaRE(RE), 'RE deveria ser válida');

  Ano := FormatDateTime('YY', Date);
  RE := Ano + '9999999'+ '001';
  Check(ValidaRE(RE), 'RE deveria ser válida');
end;

procedure ValidaRETest.Serie_Incorreta_RetornaFalso;
var
  Ano, RE: string;
begin
  Ano := FormatDateTime('YY', Date);
  RE := Ano + '0000001'+ '000';
  CheckFalse(ValidaRE(RE), 'Série 000 deveria ser inválida');
end;

{ ValidaDrawbackTest }

procedure ValidaDrawbackTest.Ano_Incorreto_RetornaFalso;
var
  Ano, Drawback: string;
begin
  Ano := FormatDateTime('YYYY', EncodeDate(YearOf(Date)+2, 1, 1));

  Drawback := Ano + '000001';
  Drawback := Drawback + Modulo11(Copy(Drawback, 3, 8));
  CheckFalse(ValidaDrawback(Drawback), 'Ano superior deveria ser inválido');

  Ano := FormatDateTime('YYYY', EncodeDate(YearOf(Date)-2, 1, 1));
  Drawback := Ano + '000001';
  Drawback := Drawback + Modulo11(Copy(Drawback, 3, 8));
  CheckFalse(ValidaDrawback(Drawback), 'Ano inferior deveria ser inválido');
end;

procedure ValidaDrawbackTest.ComLetras_RetornaFalso;
var
  Ano, Drawback, Digito: string;
begin
  Ano := FormatDateTime('YYYY', Date);
  Drawback := Ano + 'A00001';
  Digito :=  Modulo11(Copy(Drawback, 3, 8));
  Drawback := Drawback+Digito;
  CheckFalse(ValidaDrawback(Drawback));
end;

procedure ValidaDrawbackTest.Diferente_de_11_Caracteres_RetornaFalso;
var
  Ano, Drawback: string;
begin
  Ano := FormatDateTime('YYYY', Date);
  Drawback := Ano + '0000001';
  Drawback := Drawback + Modulo11(Copy(Drawback, 3, 9));
  CheckFalse(ValidaDrawback(Drawback), 'Drawback com mais de 11 caracteres deveria ser inválido');

  Ano := FormatDateTime('YYYY', Date);
  Drawback := Ano + '00001';
  Drawback := Drawback + Modulo11(Copy(Drawback, 3, 7));
  CheckFalse(ValidaDrawback(Drawback), 'Drawback com menos de 11 caracteres deveria ser inválido');
end;

procedure ValidaDrawbackTest.Digito_Incorreto_RetornaFalso;
var
  Ano, Drawback, Digito: string;
begin
  Ano := FormatDateTime('YYYY', Date);
  Drawback := Ano + '000001';
  Digito :=  Modulo11(Copy(Drawback, 3, 8));
  if Digito = '0' then
    Digito := '1'
  else Digito := '0';
  Drawback := Drawback+Digito;
  CheckFalse(ValidaDrawback(Drawback), 'Drawback com Digito Verificador incorreto deveria ser inválido');
end;

procedure ValidaDrawbackTest.Drawback_Valido_RetornaTrue;
var
  Ano, Drawback, Digito: string;
begin
  Ano := FormatDateTime('YYYY', Date);
  Drawback := Ano + '000001';
  Digito :=  Modulo11(Copy(Drawback, 3, 8));
  Drawback := Drawback+Digito;
  Check(ValidaDrawback(Drawback));
end;

{ ValidaRECOPITest }

procedure ValidaRECOPITest.ComLetras_RetornaFalso;
var
  RECOPI, Digito: string;
begin
  //RECOPI := '201701011700309999'; //aaaammddhhmmssffffDD
  RECOPI := '20170101A700309999'; //aaaammddhhmmssffffDD
  Digito := Modulo11(RECOPI, 1, 18);
  RECOPI := RECOPI + Digito;
  Digito := Modulo11(RECOPI, 1, 19);
  RECOPI := RECOPI + Digito;
  CheckFalse(ValidaRECOPI(RECOPI));
end;

procedure ValidaRECOPITest.Diferente_de_20_Caracteres_RetornaFalso;
var
  RECOPI, Digito: string;
begin
  RECOPI := '201701011700309999'; //aaaammddhhmmssffffDD
  Digito := Modulo11(RECOPI, 1, 18);
  RECOPI := RECOPI + Digito;
  Digito := Modulo11(RECOPI, 1, 19);
  RECOPI := RECOPI + Digito + '00';
  CheckFalse(ValidaRECOPI(RECOPI));
end;

procedure ValidaRECOPITest.Digito_Incorreto_RetornaFalso;
var
  RECOPI, RECOPIFinal, Digito1, Digito2: string;
begin
  RECOPI := '201701011700309999'; //aaaammddhhmmssffffDD

  Digito1 := Modulo11(RECOPI, 1, 18);
  if Digito1 = '0' then
    Digito1 := '1'
  else Digito1 := '0';
  RECOPIFinal := RECOPI + Digito1;
  Digito2 := Modulo11(RECOPIFinal, 1, 19);
  RECOPIFinal := RECOPIFinal + Digito2;
  CheckFalse(ValidaRECOPI(RECOPIFinal));

  Digito1 := Modulo11(RECOPI, 1, 18);
  RECOPIFinal := RECOPI + Digito1;
  Digito2 := Modulo11(RECOPIFinal, 1, 19);
  if Digito2 = '0' then
    Digito2 := '1'
  else Digito2 := '0';
  RECOPIFinal := RECOPIFinal + Digito2;
  CheckFalse(ValidaRECOPI(RECOPIFinal));
end;

procedure ValidaRECOPITest.RECOPI_Valido_RetornaTrue;
var
  RECOPI, Digito: string;
begin
  RECOPI := '201701011700309999'; //aaaammddhhmmssffffDD
  Digito := Modulo11(RECOPI, 1, 18);
  RECOPI := RECOPI + Digito;
  Digito := Modulo11(RECOPI, 1, 19);
  RECOPI := RECOPI + Digito;
  Check(ValidaRECOPI(RECOPI));
end;

{ ExtraiURITest }

procedure EncontrarURITest.Nao_Possui_Aspa_Final_LevantaExcecao;
const
  XML_URI = '<xml Id="teste>';
  MSG_ESPERADA = 'Não encontrei inicio do URI: aspas final';
begin
  try
    EncontrarURI(XML_URI);
  except on e: Exception do
    begin
      Check(e is EACBrDFeException);
      CheckEquals(ACBrStr(MSG_ESPERADA), e.Message);
    end
  end;
end;

procedure EncontrarURITest.Nao_Possui_Aspa_Inicial_LevantaExcecao;
const
  XML_URI = '<xml Id=teste>';
  MSG_ESPERADA = 'Não encontrei inicio do URI: aspas inicial';
begin
  try
    EncontrarURI(XML_URI);
  except on e: Exception do
    begin
      Check(e is EACBrDFeException);
      CheckEquals(ACBrStr(MSG_ESPERADA), e.Message);
    end
  end;
end;

procedure EncontrarURITest.Nao_Possui_ID_RetornaVazio;
const
  XML_URI = '<xml>';
begin
  CheckEquals('', EncontrarURI(XML_URI));
end;

procedure EncontrarURITest.URI_Correta_Retorna_URI;
const
  XML_URI = '<xml Id="teste">';
begin
  CheckEquals('teste', EncontrarURI(XML_URI));
end;

{ XmlEstaAssinadoTest }

procedure XmlEstaAssinadoTest.XML_Assinado_CaixaAlta_RetornaTrue;
const
  XML = '<SIGNATURE></SIGNATURE>';
begin
  Check(XmlEstaAssinado(XML));
end;

procedure XmlEstaAssinadoTest.XML_Assinado_CaixaBaixa_RetornaTrue;
const
  XML = '<signature></signature>';
begin
  Check(XmlEstaAssinado(XML));
end;

procedure XmlEstaAssinadoTest.XML_Nao_Assinado_RetornaFalso;
const
  XML = '<xml></xml>';
begin
  CheckFalse(XmlEstaAssinado(XML));
end;

{ URLServicosTest }

procedure URLServicosTest.SetUp;
var
  Ini: TIniFile;
begin
  inherited;
  NomeArquivo := ExtractFilePath(ParamStr(0)) + 'ACBrNFeServicos.ini';

  Ini := TIniFile.Create(NomeArquivo);
  try
    Ini.WriteString('NFCe_PR_H', 'URL-QRCode', 'http://www.dfeportal.fazenda.pr.gov.br/dfe-portal/rest/servico/consultaNFCe');
    Ini.WriteString('NFCe_PR_H', 'URL-QRCode_4.00', 'http://www.fazenda.pr.gov.br/nfce/qrcode');
    Ini.WriteString('NFCe_PR_H', 'URL-ConsultaNFCe', 'http://www.fazenda.pr.gov.br');
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

end;

procedure URLServicosTest.TearDown;
begin
  inherited;
  if FileExists(NomeArquivo) then
    DeleteFile(NomeArquivo);
end;

procedure URLServicosTest.CheckURLNFCe;
var
  ACBrNFe: TACBrNFe;
  URL: string;
begin

  ACBrNFe := TACBrNFe.Create(nil);
  try
    URL := ACBrNFe.GetURLQRCode(41, taHomologacao, '', '', Now, 100.00, 18.00, '', 4.00);
    URL := Copy(URL, 1, Pos('?', URL) - 1);
    CheckEquals('http://www.fazenda.pr.gov.br/nfce/qrcode', URL, 'URLQRCode');

    URL := ACBrNFe.GetURLQRCode(41, taHomologacao, '', '', Now, 100.00, 18.00, '', 3.10);
    URL := Copy(URL, 1, Pos('?', URL) - 1);
    CheckEquals('http://www.dfeportal.fazenda.pr.gov.br/dfe-portal/rest/servico/consultaNFCe', URL, 'URLQRCode');

    CheckEquals('http://www.fazenda.pr.gov.br', ACBrNFe.GetURLConsultaNFCe(41, taHomologacao, 4.00), 'URLConsultaNFCe');
    CheckEquals('http://www.fazenda.pr.gov.br', ACBrNFe.GetURLConsultaNFCe(41, taHomologacao, 3.10), 'URLConsultaNFCe');
  finally
    ACBrNFe.Free;
  end;

end;

initialization
  _RegisterTest('ACBrDFe.ACBrDFeUtil', FormatarNumeroDocumentoFiscalTest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', FormatarChaveAcessoTest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', FormatarNumeroDocumentoFiscalNFSeTest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', ValidaUFCidadeTest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', ValidaDIDSITest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', ValidaDIRETest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', ValidaRETest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', ValidaDrawbackTest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', ValidaRECOPITest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', EncontrarURITest);
  _RegisterTest('ACBrDFe.ACBrDFeUtil', XmlEstaAssinadoTest);
  _RegisterTest('ACBrDFe.URLServicos', URLServicosTest);

end.

