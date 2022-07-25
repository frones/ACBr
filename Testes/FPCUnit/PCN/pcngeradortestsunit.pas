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
    procedure CampoString_ValorAcentuado_GeraSemAcento;
    procedure CampoString_ValorAcentuado_NaoGeraAlerta;
    procedure CampoString_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoString_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampoString_ValorLongo_GeraCampo_GeraAlerta;
    procedure CampoInt_ValorQualquer_OcorrenciaMenosUm_NaoGeraTag;
    procedure CampoInt_ValorZero_OcorrenciaZero_NaoGeraTag;
    procedure CampoInt_ValorString_OcorrenciaUm_GeraTagComValorZero;
    procedure CampoInt_ValorPreenchido255_OcorrenciaUm_GeraTag;
    procedure CampoInt_ValorPreenchido255_OcorrenciaUm_NaoGeraAlerta;
    procedure CampoInt_ValorPreenchidoLimiteInteiro_OcorrenciaUm_GeraTag;
    procedure CampoInt_ValorPreenchidoLimiteInteiro_TamanhoMaximo5_GeraAlerta;
    procedure CampoInt64_ValorQualquer_OcorrenciaMenosUm_NaoGeraTag;
    procedure CampoInt64_ValorZero_OcorrenciaZero_NaoGeraTag;
    procedure CampoInt64_ValorString_OcorrenciaUm_GeraTagComValorZero;
    procedure CampoInt64_ValorPreenchido255_OcorrenciaUm_GeraTag;
    procedure CampoInt64_ValorPreenchido255_OcorrenciaUm_NaoGeraAlerta;
    procedure CampoInt64_ValorPreenchidoLimiteInteiro64_OcorrenciaUm_GeraTag;
    procedure CampoInt64_ValorPreenchidoLimiteInteiro_TamanhoMaximo15_GeraAlerta;
    procedure CampoData_ValorValido_GeraTag;
    procedure CampoData_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoData_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampoDataCFe_ValorValido_GeraTag;
    procedure CampoDataCFe_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoDataCFe_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampoDataHora_ValorValido_GeraTag;
    procedure CampoDataHora_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoDataHora_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampoDataHoraCFe_ValorValido_GeraTag;
    procedure CampoDataHoraCFe_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoDataHoraCFe_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampoDataVencimento_ValorValido_GeraTag;
    procedure CampoDataVencimento_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoDataVencimento_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampoHora_ValorValido_GeraTag;
    procedure CampoHora_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoHora_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampoHoraCFe_ValorValido_GeraTag;
    procedure CampoHoraCFe_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoHoraCFe_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampoBoolStr_ValorValido_GeraTag;
    //procedure CampoBoolStr_ValorVazio_Ocorrencia1_GeraTag;
    //procedure CampoBoolStr_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampotcEsp_ValorValido_GeraTag;
    procedure CampotcEsp_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampotcEsp_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampotcEsp_ValorContemLetras_GeraTag_GeraAlterta;
    procedure CampotcStrOrig_ValorValido_GeraTag;
    procedure CampotcStrOrig_ValorAcentuadoComEspacos_GeraSemAcento;
    procedure CampotcStrOrig_ValorAcentuadoComEspacos_NaoGeraAlerta;
    procedure CampotcStrOrig_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampotcStrOrig_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampotcStrOrig_ValorSoComEspacos_Ocorrencia1_GeraTag;
    procedure CampotcStrOrig_ValorSoComEspacos_OcorrenciaZero_GeraTag;
    procedure CampotcStrOrig_ValorLongo_GeraCampo_GeraAlerta;
    procedure CampotcStrOrig_ValorLongoComEspacos_GeraCampo_GeraAlerta;
    procedure CampotcNumStr_ValorValido_GeraTag;
    procedure CampotcNumStr_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampotcNumStr_ValorVazio_OcorrenciaZero_NaoGeraTag;
    procedure CampotcNumStr_ValorValido_MinAdicionaZerosAEsquerda;
    procedure CampoFloat2Decimais_ValorSimples_GeraCampo;
    procedure CampoFloat2Decimais_ValorSimples_NaoGeraAlerta;
    procedure CampoDecimal2Casas_ValorValido_GeraTag;
    procedure CampoDecimal2Casas_ValorVazio_Ocorrencia1_GeraTag;
    procedure CampoDecimal2Casas_ValorVazio_OcorrenciaZero_NaoGeraTag;

{
procedure CampoXXX_ValorValido_GeraTag;
procedure CampoXXX_ValorVazio_Ocorrencia1_GeraTag;
procedure CampoXXX_ValorVazio_OcorrenciaZero_NaoGeraTag;
}//                       x      x    x         x        x
    {  TpcnTipoCampo = (tcStr, tcInt, tcDat, tcDatHor, tcEsp, tcDe2, tcDe3, tcDe4,
                                                          x      x         x         X
                   tcDe5, tcDe6, tcDe7, tcDe8, tcDe10, tcHor, tcDatCFe, tcHorCFe, tcDatVcto,
                        x           x          x          x        x
                   tcDatHorCFe, tcBoolStr, tcStrOrig, tcNumStr, tcInt64);}

  end;

implementation

uses
  pcnconversao, ACBrUtil.Strings;

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

procedure pcnGeradorTest.CampoString_ValorAcentuado_GeraSemAcento;
var
  a: string;
begin
  UmGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, 'EsseTextoÉUmRecibo');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>EsseTextoEUmRecibo</Recibo>', a);
end;

procedure pcnGeradorTest.CampoString_ValorAcentuado_NaoGeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, 'EsseTextoÉUmRecibo');
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta.');
end;

procedure pcnGeradorTest.CampoString_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, '');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampoString_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 0, '');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoString_ValorLongo_GeraCampo_GeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcStr, '', 'Recibo', 1, 25, 1, '123456789012345678901234567890');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>123456789012345678901234567890</Recibo>', a);
  a := UmGerador.ListaDeAlertas.Text;
  CheckNotEquals('', a, 'Deveria conter um alerta sobre o passar do tamanho máximo.');
end;

procedure pcnGeradorTest.CampoInt_ValorQualquer_OcorrenciaMenosUm_NaoGeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcInt, '', 'Recibo', 1, 30, -1, 1337);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoInt_ValorZero_OcorrenciaZero_NaoGeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcInt, '', 'Recibo', 1, 30, 0, 0);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoInt_ValorString_OcorrenciaUm_GeraTagComValorZero;
var
  a: string;
begin
  UmGerador.wCampo(tcInt, '', 'Recibo', 1, 30, 1, 'VaiVirarZero');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>0</Recibo>', a);
end;

procedure pcnGeradorTest.CampoInt_ValorPreenchido255_OcorrenciaUm_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcInt, '', 'Recibo', 1, 30, 1, 255);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>255</Recibo>', a);
end;

procedure pcnGeradorTest.CampoInt_ValorPreenchido255_OcorrenciaUm_NaoGeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcInt, '', 'Recibo', 1, 30, 1, 255);
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta.');

end;

procedure pcnGeradorTest.CampoInt_ValorPreenchidoLimiteInteiro_OcorrenciaUm_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcInt, '', 'Recibo', 1, 30, 1, 2147483647);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>2147483647</Recibo>', a);
end;

procedure pcnGeradorTest.CampoInt_ValorPreenchidoLimiteInteiro_TamanhoMaximo5_GeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcInt, '', 'Recibo', 1, 5, 1, 2147483647);
  //a := UmGerador.ArquivoFormatoXML;
  //CheckEquals('<Recibo>2147483647</Recibo>', a);
  a := UmGerador.ListaDeAlertas.Text;
  CheckNotEquals('', a, 'Deveria conter um alerta sobre o passar do tamanho máximo.');
end;

procedure pcnGeradorTest.CampoInt64_ValorQualquer_OcorrenciaMenosUm_NaoGeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcInt64, '', 'Recibo', 1, 30, -1, 1337);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoInt64_ValorZero_OcorrenciaZero_NaoGeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcInt64, '', 'Recibo', 1, 30, 0, 0);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoInt64_ValorString_OcorrenciaUm_GeraTagComValorZero;
var
  a: string;
begin
  UmGerador.wCampo(tcInt64, '', 'Recibo', 1, 30, 1, 'VaiVirarZero');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>0</Recibo>', a);
end;

procedure pcnGeradorTest.CampoInt64_ValorPreenchido255_OcorrenciaUm_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcInt64, '', 'Recibo', 1, 30, 1, 255);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>255</Recibo>', a);
end;

procedure pcnGeradorTest.CampoInt64_ValorPreenchido255_OcorrenciaUm_NaoGeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcInt64, '', 'Recibo', 1, 30, 1, 255);
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta.');

end;

procedure pcnGeradorTest.CampoInt64_ValorPreenchidoLimiteInteiro64_OcorrenciaUm_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcInt64, '', 'Recibo', 1, 30, 1, 9223372036854775807);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>9223372036854775807</Recibo>', a);
end;

procedure pcnGeradorTest.CampoInt64_ValorPreenchidoLimiteInteiro_TamanhoMaximo15_GeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcInt64, '', 'Recibo', 1, 15, 1, 9223372036854775807);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>9223372036854775807</Recibo>', a);
  a := UmGerador.ListaDeAlertas.Text;
  CheckNotEquals('', a, 'Deveria conter um alerta sobre o passar do tamanho máximo.');
end;

procedure pcnGeradorTest.CampoData_ValorValido_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcDat, '', 'Recibo', 1, 30, 1, EncodeDate(2022,03,14));
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>2022-03-14</Recibo>', a);
end;

procedure pcnGeradorTest.CampoData_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
  b: TDate;
begin
  UmGerador.wCampo(tcDat, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampoData_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
  b: TDate;
begin
  UmGerador.wCampo(tcDat, '', 'Recibo', 1, 30, 0, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoDataCFe_ValorValido_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcDatCFe, '', 'Recibo', 1, 30, 1, EncodeDate(2022,03,14));
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>20220314</Recibo>', a);
end;

procedure pcnGeradorTest.CampoDataCFe_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
  b: TDate;
begin
  UmGerador.wCampo(tcDatCFe, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampoDataCFe_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
  b: TDate;
begin
  UmGerador.wCampo(tcDatCFe, '', 'Recibo', 1, 30, 0, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoDataHora_ValorValido_GeraTag;
var
  a: string;
  b: TDateTime;
begin
  b := EncodeDate(2022,03,14) + EncodeTime(11,12,13,14);
  UmGerador.wCampo(tcDatHor, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>2022-03-14T11:12:13</Recibo>', a);
end;

procedure pcnGeradorTest.CampoDataHora_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
  b: TDateTime;
begin
  UmGerador.wCampo(tcDatHor, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampoDataHora_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
  b: TDateTime;
begin
  UmGerador.wCampo(tcDatHor, '', 'Recibo', 1, 30, 0, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoDataHoraCFe_ValorValido_GeraTag;
var
  a: string;
  b: TDateTime;
begin
  b := EncodeDate(2022,03,14) + EncodeTime(11,12,13,14);
  UmGerador.wCampo(tcDatHorCFe, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>20220314111213</Recibo>', a);
end;

procedure pcnGeradorTest.CampoDataHoraCFe_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
  b: TDateTime;
begin
  UmGerador.wCampo(tcDatHorCFe, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampoDataHoraCFe_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
  b: TDateTime;
begin
  UmGerador.wCampo(tcDatHorCFe, '', 'Recibo', 1, 30, 0, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoDataVencimento_ValorValido_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcDatVcto, '', 'Recibo', 1, 30, 1, EncodeDate(2022,03,14));
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>14/03/2022</Recibo>', a);
end;

procedure pcnGeradorTest.CampoDataVencimento_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
  b: TDate;
begin
  UmGerador.wCampo(tcDatVcto, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampoDataVencimento_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
  b: TDate;
begin
  UmGerador.wCampo(tcDatVcto, '', 'Recibo', 1, 30, 0, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoHora_ValorValido_GeraTag;
var
  a: string;
  b: TDateTime;
begin
  b := EncodeTime(11,12,13,14);
  UmGerador.wCampo(tcHor, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>11:12:13</Recibo>', a);
end;

procedure pcnGeradorTest.CampoHora_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
  b: TDateTime;
begin
  UmGerador.wCampo(tcHor, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampoHora_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
  b: TDateTime;
begin
  UmGerador.wCampo(tcHor, '', 'Recibo', 1, 30, 0, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoHoraCFe_ValorValido_GeraTag;
var
  a: string;
  b: TDateTime;
begin
  b := EncodeTime(11,12,13,14);
  UmGerador.wCampo(tcHorCFe, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>111213</Recibo>', a);
end;

procedure pcnGeradorTest.CampoHoraCFe_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
  b: TDateTime;
begin
  UmGerador.wCampo(tcHorCFe, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampoHoraCFe_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
  b: TDateTime;
begin
  UmGerador.wCampo(tcHorCFe, '', 'Recibo', 1, 30, 0, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoBoolStr_ValorValido_GeraTag;
var
  a: string;
  b: Boolean;
begin
  b := True;
  UmGerador.wCampo(tcBoolStr, '', 'Recibo', 1, 1, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>true</Recibo>', a);

end;

procedure pcnGeradorTest.CampotcEsp_ValorValido_GeraTag;
var
  a,b: string;
begin
  b := '123';
  UmGerador.wCampo(tcEsp, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>123</Recibo>', a);
end;

procedure pcnGeradorTest.CampotcEsp_ValorVazio_Ocorrencia1_GeraTag;
var
  a,b: string;
begin
  b := '';
  UmGerador.wCampo(tcEsp, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampotcEsp_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a,b: string;
begin
  b := '';
  UmGerador.wCampo(tcEsp, '', 'Recibo', 1, 30, 0, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;


procedure pcnGeradorTest.CampotcEsp_ValorContemLetras_GeraTag_GeraAlterta;
var
  a: string;
begin
  UmGerador.wCampo(tcEsp, '', 'Recibo', 1, 30, 0, '123a');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>123a</Recibo>', a);
  a := UmGerador.ListaDeAlertas.Text;
  CheckNotEquals('', a, 'Deveria conter um alerta sobre o valor não ser somente números.');
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorValido_GeraTag;
var
  a,b: string;
begin
  b := '123abc';
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>123abc</Recibo>', a);
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorAcentuadoComEspacos_GeraSemAcento;
var
  a,b: string;
begin
  b := ' 123 abc áéíàêãç ';
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>123 abc aeiaeac</Recibo>', a);
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorAcentuadoComEspacos_NaoGeraAlerta;
var
  a,b: string;
begin
  b := ' 123 abc áéíàêãç ';
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 30, 1, b);
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta.');
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 30, 1, '');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo/>', a);
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 30, 0, '');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorSoComEspacos_Ocorrencia1_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 30, 1, '    ');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo></Recibo>', a);
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorSoComEspacos_OcorrenciaZero_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 30, 0, '    ');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo></Recibo>', a);
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorLongo_GeraCampo_GeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 25, 1, '123456789012345678901234567890');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>123456789012345678901234567890</Recibo>', a);
  a := UmGerador.ListaDeAlertas.Text;
  CheckNotEquals('', a, 'Deveria conter um alerta sobre o passar do tamanho máximo.');
end;

procedure pcnGeradorTest.CampotcStrOrig_ValorLongoComEspacos_GeraCampo_GeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcStrOrig, '', 'Recibo', 1, 30, 1, '1234567890 1234567890 1234567890');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>1234567890 1234567890 1234567890</Recibo>', a);
  a := UmGerador.ListaDeAlertas.Text;
  CheckNotEquals('', a, 'Deveria conter um alerta sobre o passar do tamanho máximo.');
end;

procedure pcnGeradorTest.CampotcNumStr_ValorValido_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcNumStr, '', 'Recibo', 1, 14, 1, OnlyNumber(FormatFloat('0.00', 123.45)));
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>12345</Recibo>', a);
end;

procedure pcnGeradorTest.CampotcNumStr_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcNumStr, '', 'Recibo', 1, 14, 1, 0);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>0</Recibo>', a);
end;

procedure pcnGeradorTest.CampotcNumStr_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcNumStr, '', 'Recibo', 1, 14, 0, '');
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampotcNumStr_ValorValido_MinAdicionaZerosAEsquerda;
var
  a: string;
begin
  UmGerador.wCampo(tcNumStr, '', 'Recibo', 14, 14, 1, OnlyNumber(FormatFloat('0.00', 654.32)));
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>00000000065432</Recibo>', a);
end;


procedure pcnGeradorTest.CampoFloat2Decimais_ValorSimples_GeraCampo;
var
  a: string;
begin
  UmGerador.wCampo(tcDe2, '', 'Recibo', 1, 36, 1, 1.10);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>1.10</Recibo>', a);
end;

procedure pcnGeradorTest.CampoFloat2Decimais_ValorSimples_NaoGeraAlerta;
var
  a: string;
begin
  UmGerador.wCampo(tcDe2, '', 'Recibo', 1, 36, 1, 1.10);
  a := UmGerador.ListaDeAlertas.Text;
  CheckEquals('', a, 'Não deveria conter um alerta.');
end;

procedure pcnGeradorTest.CampoDecimal2Casas_ValorValido_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcDe2, '', 'Recibo', 1, 14, 1, 123.45);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('<Recibo>123.45</Recibo>', a);

end;

procedure pcnGeradorTest.CampoDecimal2Casas_ValorVazio_Ocorrencia1_GeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcDe2, '', 'Recibo', 1, 14, 0, 0.0);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;

procedure pcnGeradorTest.CampoDecimal2Casas_ValorVazio_OcorrenciaZero_NaoGeraTag;
var
  a: string;
begin
  UmGerador.wCampo(tcDe2, '', 'Recibo', 1, 14, 0, 0);
  a := UmGerador.ArquivoFormatoXML;
  CheckEquals('', a);
end;



initialization

  _RegisterTest('ACBrPCN.PCNGerador', pcnGeradorTest);
end.

