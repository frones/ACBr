unit pcnGeradorTestsUnit;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, pcnGerador,
  ACBrTests.Util;


type

  { pcnGeradorTest }

  pcnGeradorTest= class(TTestCase)
  private
    UmGerador: TGerador;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SemDados_SalvaArquivo_Vazio;
    procedure ComUm_CPFBrasil_SalvaArquivoXML_ComDados;
    procedure SalvaArquivoTXT_ComDados_Iguais;
    procedure wCampoCPF_CPF_NaoBrasil_TagVazia;
    procedure wCampoCPF_CPF_NaoBrasil_SemAlerta;
    procedure wCampoCPF_CPFInvalido_Brasil_TagGerada;
    procedure wCampoCPF_CPFInvalido_Brasil_GeraAlerta;
    procedure wCampoCPF_CPFValido_Brasil_TagGerada;
    procedure wCampoCPF_CPFValido_Brasil_SemAlerta;
    procedure wCampoCPF_CPFFormatadoValido_Brasil_TagGeradaSemFormatacao;
    procedure wCampoCPF_CPFFormatadoValido_Brasil_SemAlerta;
    procedure GeraGrupoComCampoString_ValorAcentuado_GeraSemAcento;
    procedure GeraGrupoComCampoString_ValorAcentuado_NaoGeraAlerta;
    procedure CampoValorFloat2Decimais_GeraCampo;
    procedure CampoValorFloat2Decimais_NaoGeraAlerta;
  end;

implementation

uses
  pcnconversao;

const
  NomeArqTemp = '.\tempfile.txt';

procedure pcnGeradorTest.SetUp;
begin
  inherited SetUp;
  UmGerador := TGerador.Create;
end;

procedure pcnGeradorTest.TearDown;
begin
  UmGerador.Free;
  inherited TearDown;
end;

procedure pcnGeradorTest.SemDados_SalvaArquivo_Vazio;
var
  Result :Boolean;
  umaStrlst: TStringList;
  a: string;
begin
  Result := UmGerador.SalvarArquivo(NomeArqTemp);

  umaStrlst:= TStringList.Create;
  try
    umaStrlst.LoadFromFile(NomeArqTemp);
    a := umaStrlst.Text;
    Result := Result and
              ((a = '') or (a=sLineBreak));
    CheckTrue(Result);
  finally
    umaStrlst.Free;
  end;

end;

procedure pcnGeradorTest.ComUm_CPFBrasil_SalvaArquivoXML_ComDados;
var
  Result :Boolean;
  umaStrlst: TStringList;
  a: string;
begin
  UmGerador.wCampoCPF('#089', '12345678909', 1058, False);

  Result := UmGerador.SalvarArquivo(NomeArqTemp, fgXML);

  umaStrlst:= TStringList.Create;
  try
    umaStrlst.LoadFromFile(NomeArqTemp);
    a := umaStrlst.Text;
    Result := Result and
              ((a = '<CPF>12345678909</CPF>') or (a= ('<CPF>12345678909</CPF>'+sLineBreak)));
    CheckTrue(Result);
  finally
    umaStrlst.Free;
  end;
end;

procedure pcnGeradorTest.SalvaArquivoTXT_ComDados_Iguais;
var
  Result :Boolean;
  umaStrlst: TStringList;
  a: string;
begin
  UmGerador.ArquivoFormatoTXT := '<CPF>12345678909</CPF>';

  Result := UmGerador.SalvarArquivo(NomeArqTemp, fgTXT);

  umaStrlst:= TStringList.Create;
  try
    umaStrlst.LoadFromFile(NomeArqTemp);
    a := umaStrlst.Text;
    Result := Result and
              ((a = '<CPF>12345678909</CPF>') or (a= ('<CPF>12345678909</CPF>'+sLineBreak)));
    CheckTrue(Result);
  finally
    umaStrlst.Free;
  end;

end;

procedure pcnGeradorTest.wCampoCPF_CPF_NaoBrasil_TagVazia;
var
  a: string;
begin
  UmGerador.wCampoCPF('#089', '11111111111', 1337, False);
  a := UmGerador.ArquivoFormatoXML;
  CheckTrue(a = '<CPF/>', 'Deveria ser vazio.');
end;

procedure pcnGeradorTest.wCampoCPF_CPF_NaoBrasil_SemAlerta;
var
  a: string;
begin
  UmGerador.wCampoCPF('#089', '11111111111', 1337, False);
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deve ter alerta');
end;

procedure pcnGeradorTest.wCampoCPF_CPFInvalido_Brasil_TagGerada;
var
  a: string;
begin
  UmGerador.wCampoCPF('#089', '11111111111', 1058, False);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<CPF>11111111111</CPF>', a);
end;

procedure pcnGeradorTest.wCampoCPF_CPFInvalido_Brasil_GeraAlerta;
var
  a: string;
begin
  UmGerador.wCampoCPF('#089', '11111111111', 1058, False);
  a := UmGerador.ListaDeAlertas.Text;
  CheckNotEquals('', a, 'Deveria conter um alerta sobre o CPF estar inválido.');
end;

procedure pcnGeradorTest.wCampoCPF_CPFValido_Brasil_TagGerada;
var
  a: string;
begin
  UmGerador.wCampoCPF('#089', '12345678909', 1058, False);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<CPF>12345678909</CPF>', a);
end;

procedure pcnGeradorTest.wCampoCPF_CPFValido_Brasil_SemAlerta;
var
  a: string;
begin
  UmGerador.wCampoCPF('#089', '12345678909', 1058, False);
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta sobre o CPF estar inválido.');
end;

procedure pcnGeradorTest.wCampoCPF_CPFFormatadoValido_Brasil_TagGeradaSemFormatacao;
var
  a: string;
begin
  UmGerador.wCampoCPF('#089', '123.456.789-09', 1058, False);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<CPF>12345678909</CPF>', a);
end;

procedure pcnGeradorTest.wCampoCPF_CPFFormatadoValido_Brasil_SemAlerta;
var
  a: string;
begin
  UmGerador.wCampoCPF('#089', '123.456.789-09', 1058, False);
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta sobre o CPF estar inválido.');
end;

procedure pcnGeradorTest.GeraGrupoComCampoString_ValorAcentuado_GeraSemAcento;
var
  a: string;
begin
  UmGerador.wGrupo('Mensagem');
  UmGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, 'EsseTextoÉUmRecibo');
  UmGerador.wGrupo('/Mensagem');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Mensagem><Recibo>EsseTextoEUmRecibo</Recibo></Mensagem>', a);
end;

procedure pcnGeradorTest.GeraGrupoComCampoString_ValorAcentuado_NaoGeraAlerta;
var
  a: string;
begin
  UmGerador.wGrupo('Mensagem');
  UmGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, 'EsseTextoÉUmRecibo');
  UmGerador.wGrupo('/Mensagem');
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta.');
end;

procedure pcnGeradorTest.CampoValorFloat2Decimais_GeraCampo;
var
  a: string;
begin
  UmGerador.wCampo(tcDe2, '', 'Recibo', 1, 36, 1, 1.10);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>1.10</Recibo>', a);
end;

procedure pcnGeradorTest.CampoValorFloat2Decimais_NaoGeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcDe2, '', 'Recibo', 1, 36, 1, 1.10);
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta.');
end;


initialization

  _RegisterTest('ACBrPCN.PCNGerador', pcnGeradorTest);
end.

