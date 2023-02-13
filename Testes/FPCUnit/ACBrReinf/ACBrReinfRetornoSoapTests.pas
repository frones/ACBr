unit ACBrReinfRetornoSoapTests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBrReinf, ACBrReinfWebServices, pcnReinfR9005;

const
  ARQUIVO_RETORNO_SOAP        = '..\..\..\..\Recursos\Reinf\RetornoSoap.xml';
  ARQUIVO_RETORNO             = '..\..\..\..\Recursos\Reinf\Retorno.xml';

type

  { TACBrReinfRetornoSoapTests }

  TACBrReinfRetornoSoapTests = class(TTestCase)
  private
    FACBrReinf: TACBrReinf;
  public
    procedure Setup;override;
    procedure TearDown;override;
  published
    procedure LoadFromFile_TratarResposta;
    procedure evtRet_LerXML;

  end;

implementation

uses
  ACBrUtil.XMLHTML, ACBrUtil.FilesIO, pcnLeitor, ACBrUtil.Strings;

{ TACBrReinfRetornoSoapTests }

procedure TACBrReinfRetornoSoapTests.Setup;
begin
  FACBrReinf := TACBrReinf.Create(nil);
  inherited Setup;
end;

procedure TACBrReinfRetornoSoapTests.TearDown;
begin
  FACBrReinf.Free;
  inherited TearDown;
end;

procedure TACBrReinfRetornoSoapTests.LoadFromFile_TratarResposta;
var
  sXML: string;
  lStrList: TStringList;
  i: Integer;
begin
  lStrList := TStringList.Create;
  try
    lStrList.LoadFromFile(ARQUIVO_RETORNO_SOAP);
    sXML := lStrList.Text;
  finally
    lStrList.Free;
  end;
  if(Length(sXML) >= Length(sLineBreak))then
    sXML := Copy(sXML, 0, Length(sXML) - Length(sLineBreak));

  sXML := SeparaDados(sXML, 'retornoLoteEventosAssincrono');
  if(sXML<>'')then
    sXML := '<retornoLoteEventosAssincrono>' + sXML + '</retornoLoteEventosAssincrono>';

  sXML := RetornarConteudoEntre(sXML, '<retornoEventos>', '</retornoEventos');
  sXML := RetornarConteudoEntre(sXML, '<retornoEvento>', '</retornoEvento');

  sXML := InserirDeclaracaoXMLSeNecessario(sXML);

  WriteToTXT(ARQUIVO_RETORNO, sXML, False, False);

end;

procedure TACBrReinfRetornoSoapTests.evtRet_LerXML;
var
  evtRet: TevtRet;
  evtXML: TStringList;
begin
  evtRet := TevtRet.Create;
  evtXML := TStringList.Create;
  try
    evtXML.LoadFromFile(ARQUIVO_RETORNO);
    evtRet.Leitor.Arquivo := evtXML.Text;
    evtRet.LerXML;
  finally
    evtXML.Free;
    evtRet.Free;
  end;
end;

initialization
  _RegisterTest('ACBrReinfRetornoSoapTests', TACBrReinfRetornoSoapTests);

end.

