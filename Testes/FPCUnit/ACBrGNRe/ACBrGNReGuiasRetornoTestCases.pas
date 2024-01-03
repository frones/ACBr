unit ACBrGNReGuiasRetornoTestCases;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrGNRe2, ACBrTests.Util, ACBrGNReTestConsts;

type

  { TACBrGNReGuiasRetornoTestCases }

  TACBrGNReGuiasRetornoTestCases = class(TTestCase)
  private
    FGNRe: TACBrGNRe;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LerXML_LeuTodasAsGuias;
    procedure LoadFromFile_LeuOsValoresFCP;
  end;

implementation

{ TACBrGNReGuiasRetornoTestCases }

procedure TACBrGNReGuiasRetornoTestCases.SetUp;
begin
  inherited SetUp;
  FGNRe := TACBrGNRE.Create(nil);
end;

procedure TACBrGNReGuiasRetornoTestCases.TearDown;
begin
  FGNRe.Free;
  inherited TearDown;
end;

procedure TACBrGNReGuiasRetornoTestCases.LerXML_LeuTodasAsGuias;
begin
  FGNRe.GuiasRetorno.Clear;
  FGNRe.GuiasRetorno.LoadFromFile(XMLRETORNOLOTEGNRE);

  AssertEquals('Não leu todas as Guias GNRe do XML', 2, FGNRe.GuiasRetorno.Count);
end;

procedure TACBrGNReGuiasRetornoTestCases.LoadFromFile_LeuOsValoresFCP;
begin
  FGNRe.GuiasRetorno.Clear;
  FGNRe.GuiasRetorno.LoadFromFile(XMLRETORNOLOTEGNRE);

  Check(FGNRe.GuiasRetorno.Items[0].GNRe.ValorPrincipal = 243, 'ValorPrincipal|Esperado:0,00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[0].GNRe.ValorPrincipal));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.ValorFECP = 0, 'ValorFECP|Esperado:0,00|Lido:'+FormatFloat('#0,00', FGNRe.GuiasRetorno.Items[0].GNRe.ValorFECP));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.ValorICMS = 243, 'ValorICMS|Esperado:0,00|Lido:'+FormatFloat('#0,00', FGNRe.GuiasRetorno.Items[0].GNRe.ValorICMS));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.ValorFCP = 0, 'ValorFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[0].GNRe.ValorFCP));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.Multa = 0, 'MultaFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[0].GNRe.Multa));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.MultaFCP = 0, 'MultaFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[0].GNRe.MultaFCP));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.Juros = 0, 'JurosFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[0].GNRe.Juros));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.JurosFCP = 0, 'JurosFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[0].GNRe.JurosFCP));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.AtualizacaoMonetaria = 0, 'AtualizacaoMonetariaFCP|Esperado:0.00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[0].GNRe.AtualizacaoMonetaria));
  Check(FGNRe.GuiasRetorno.Items[0].GNRe.AtualizacaoMonetariaFCP = 0, 'AtualizacaoMonetariaFCP|Esperado:0.00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[0].GNRe.AtualizacaoMonetariaFCP));

  Check(FGNRe.GuiasRetorno.Items[1].GNRe.ValorPrincipal = 182.25, 'ValorPrincipal|Esperado:182,25|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[1].GNRe.ValorPrincipal));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.ValorFECP = 1.82, 'ValorFECP|Esperado:1,82|Lido:'+FormatFloat('#0,00', FGNRe.GuiasRetorno.Items[1].GNRe.ValorFECP));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.ValorICMS = 182.25, 'ValorICMS|Esperado:182,25|Lido:'+FormatFloat('#0,00', FGNRe.GuiasRetorno.Items[1].GNRe.ValorICMS));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.ValorFCP = 18.22, 'ValorFCP|Esperado:18,22|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[1].GNRe.ValorFCP));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.Multa = 0, 'MultaFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[1].GNRe.Multa));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.MultaFCP = 5.11, 'MultaFCP|Esperado:5,11|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[1].GNRe.MultaFCP));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.Juros = 0, 'JurosFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[1].GNRe.Juros));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.JurosFCP = 5.11, 'JurosFCP|Esperado:5,11|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[1].GNRe.JurosFCP));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.AtualizacaoMonetaria = 0, 'AtualizacaoMonetariaFCP|Esperado:0.00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[1].GNRe.AtualizacaoMonetaria));
  Check(FGNRe.GuiasRetorno.Items[1].GNRe.AtualizacaoMonetariaFCP = 8, 'AtualizacaoMonetariaFCP|Esperado:8.00|Lido:'+FormatFloat('#0.00', FGNRe.GuiasRetorno.Items[1].GNRe.AtualizacaoMonetariaFCP));
end;

initialization
  _RegisterTest('ACBrGNReGuiasRetornoTests', TACBrGNReGuiasRetornoTestCases);

end.

