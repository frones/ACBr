unit pgnreRetConsResLoteGNRETestCases;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, pgnreRetConsResLoteGNRE;

type

  { pgnreRetConsResLoteGNRETestCases }

  pgnreRetConsResLoteGNRETestCases = class(TTestCase)
  private
    LoteGNRE: TTResultLote_GNRE;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LerXML_LeuTodasAsGuias;
    procedure Ler_Versao_2_LeuTodosOsValores;
  end;

implementation

uses
  ACBrGNReTestConsts;

{ pgnreRetConsResLoteGNRETestCases }

procedure pgnreRetConsResLoteGNRETestCases.SetUp;
begin
  inherited SetUp;
  LoteGNRe := TTResultLote_GNRE.Create;
end;

procedure pgnreRetConsResLoteGNRETestCases.TearDown;
begin
  LoteGNRe.Free;
  inherited TearDown;
end;

procedure pgnreRetConsResLoteGNRETestCases.LerXML_LeuTodasAsGuias;
begin
  LoteGNRe.resGuia.Clear;
  LoteGNRe.Leitor.CarregarArquivo(XMLRETORNOLOTEGNRE);
  LoteGNRe.LerXml;

  AssertEquals('Não leu todas as Guias GNRe do XML', 2, LoteGNRe.resGuia.Count);
end;

procedure pgnreRetConsResLoteGNRETestCases.Ler_Versao_2_LeuTodosOsValores;
begin
  LoteGNRe.resGuia.Clear;
  LoteGNRe.Leitor.CarregarArquivo(XMLRETORNOLOTEGNRE);
  LoteGNRe.LerXml;

  Check(LoteGNRe.resGuia[0].ValorPrincipal = 243, 'ValorPrincipal|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[0].ValorPrincipal));
  Check(LoteGNRe.resGuia[0].ValorFECP = 0, 'ValorFECP|Esperado:0,00|Lido:'+FormatFloat('#0,00', LoteGNRe.resGuia[0].ValorFECP));
  Check(LoteGNRe.resGuia[0].ValorICMS = 243, 'ValorICMS|Esperado:0,00|Lido:'+FormatFloat('#0,00', LoteGNRe.resGuia[0].ValorICMS));
  Check(LoteGNRe.resGuia[0].ValorFCP = 0, 'ValorFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[0].ValorFCP));
  Check(LoteGNRe.resGuia[0].Multa = 0, 'MultaFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[0].Multa));
  Check(LoteGNRe.resGuia[0].MultaFCP = 0, 'MultaFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[0].MultaFCP));
  Check(LoteGNRe.resGuia[0].Juros = 0, 'JurosFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[0].Juros));
  Check(LoteGNRe.resGuia[0].JurosFCP = 0, 'JurosFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[0].JurosFCP));
  Check(LoteGNRe.resGuia[0].AtualizacaoMonetaria = 0, 'AtualizacaoMonetariaFCP|Esperado:0.00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[0].AtualizacaoMonetaria));
  Check(LoteGNRe.resGuia[0].AtualizacaoMonetariaFCP = 0, 'AtualizacaoMonetariaFCP|Esperado:0.00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[0].AtualizacaoMonetariaFCP));

  Check(LoteGNRe.resGuia[1].ValorPrincipal = 182.25, 'ValorPrincipal|Esperado:182,25|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].ValorPrincipal));
  Check(LoteGNRe.resGuia[1].ValorFECP = 1.82, 'ValorFECP|Esperado:1,82|Lido:'+FormatFloat('#0,00', LoteGNRe.resGuia[1].ValorFECP));
  Check(LoteGNRe.resGuia[1].ValorICMS = 182.25, 'ValorICMS|Esperado:182,25|Lido:'+FormatFloat('#0,00', LoteGNRe.resGuia[1].ValorICMS));
  Check(LoteGNRe.resGuia[1].ValorFCP = 18.22, 'ValorFCP|Esperado:18,22|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].ValorFCP));
  Check(LoteGNRe.resGuia[1].Multa = 0, 'MultaFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].Multa));
  Check(LoteGNRe.resGuia[1].MultaFCP = 5.11, 'MultaFCP|Esperado:5,11|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].MultaFCP));
  Check(LoteGNRe.resGuia[1].Juros = 0, 'JurosFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].Juros));
  Check(LoteGNRe.resGuia[1].JurosFCP = 5.11, 'JurosFCP|Esperado:5,11|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].JurosFCP));
  Check(LoteGNRe.resGuia[1].AtualizacaoMonetaria = 0, 'AtualizacaoMonetariaFCP|Esperado:0.00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].AtualizacaoMonetaria));
  Check(LoteGNRe.resGuia[1].AtualizacaoMonetariaFCP = 8, 'AtualizacaoMonetariaFCP|Esperado:8.00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].AtualizacaoMonetariaFCP));

end;


initialization
  _RegisterTest('pgnreRetConsResLoteGNRETests', pgnreRetConsResLoteGNRETestCases);

end.

