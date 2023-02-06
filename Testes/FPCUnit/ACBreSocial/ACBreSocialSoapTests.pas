unit ACBreSocialSoapTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocial, pcesConversaoeSocial;

type

  { TACBreSocialSoapTests }

  TACBreSocialSoapTests = class(TTestCase)
  private
  public
    procedure Setup;override;
    procedure TearDown;override;
  published
    procedure TipoInscCabecalhoSoapLote_ConversaoAntigaVersusFuncao_ResultadosCoindicem;
  end;

implementation

uses
  TypInfo;

{ TACBreSocialSoapTests }

procedure TACBreSocialSoapTests.Setup;
begin
  inherited Setup;
end;

procedure TACBreSocialSoapTests.TearDown;
begin
  inherited TearDown;
end;

procedure TACBreSocialSoapTests.TipoInscCabecalhoSoapLote_ConversaoAntigaVersusFuncao_ResultadosCoindicem;
var
  valorEnum: tpTpInsc;
  valorConversaoAntiga: string;
  valorConvertidoFuncao: string;
begin
  for valorEnum := Low(tpTpInsc) to High(tpTpInsc)do
  begin
    valorConversaoAntiga  := Inttostr(ord(valorEnum) + 1);
    valorConvertidoFuncao := eSTpInscricaoToStr(valorEnum);
    CheckTrue(valorConversaoAntiga = valorConvertidoFuncao, 'Erro de conversão no elemento ord('+
              GetEnumName(TypeInfo(valorEnum), ord(valorEnum))+')='+IntToStr(ord(valorEnum))+'  '+
              'Valor conversão antiga => ' + valorConversaoAntiga +'  '+
              'Valor convertido função => ' + valorConvertidoFuncao
             );
  end;
end;

end.

