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
    procedure Ler_Versao_2_LeuOsValoresFCP;
  end;

implementation

const
  XMLRETORNOLOTEGNRE = '..\..\..\..\Recursos\GNRe\GuiaGNRe_Editada_Adicionando_Valores_FCP.xml';

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

procedure pgnreRetConsResLoteGNRETestCases.Ler_Versao_2_LeuOsValoresFCP;
begin
  LoteGNRe.resGuia.Clear;
  LoteGNRe.Leitor.CarregarArquivo(XMLRETORNOLOTEGNRE);
  LoteGNRe.LerXml;

  Check(LoteGNRe.resGuia[0].ValorFCP = 0, 'ValorFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].ValorFCP));
  Check(LoteGNRe.resGuia[0].MultaFCP = 0, 'MultaFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].MultaFCP));
  Check(LoteGNRe.resGuia[0].JurosFCP = 0, 'JurosFCP|Esperado:0,00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].JurosFCP));
  Check(LoteGNRe.resGuia[0].ValorFCP = 0, 'AtualizacaoMonetariaFCP|Esperado:0.00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].AtualizacaoMonetariaFCP));

  Check(LoteGNRe.resGuia[1].ValorFCP = 18.22, 'ValorFCP|Esperado:18.22|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].ValorFCP));
  Check(LoteGNRe.resGuia[1].MultaFCP = 5.11, 'MultaFCP|Esperado:5.11|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].MultaFCP));
  Check(LoteGNRe.resGuia[1].JurosFCP = 5.11, 'JurosFCP|Esperado:5.11|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].JurosFCP));
  Check(LoteGNRe.resGuia[1].ValorFCP = 18.22, 'AtualizacaoMonetariaFCP|Esperado:8.00|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].AtualizacaoMonetariaFCP));
  Check(LoteGNRe.resGuia[1].ValorFECP = 1.82, 'ValorFECP|Esperado:1,82|Lido:'+FormatFloat('#0.00', LoteGNRe.resGuia[1].ValorFECP));

end;


initialization
  _RegisterTest('pgnreRetConsResLoteGNRETests', pgnreRetConsResLoteGNRETestCases);

end.

