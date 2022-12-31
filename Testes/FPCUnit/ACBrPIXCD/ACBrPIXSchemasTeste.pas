{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrPIXSchemasTeste;
 
{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrPIXBase, ACBrPIXSchemasCob, ACBrPIXSchemasCobV, ACBrPIXBRCode,
  ACBrPIXSchemasProblema, ACBrPIXSchemasPixConsultados,
  ACBrPIXSchemasCobsConsultadas, ACBrPIXSchemasCobsVConsultadas,
  ACBrPIXSchemasLoteCobV, ACBrPIXSchemasLotesCobVConsultadas,
  ACBrTests.Util;

type

  { TTestQRCodeEstatico }

  TTestQRCodeEstatico = class(TTestCase)
  private
    fQRStr: String;
    fQREstatico: TACBrPIXQRCodeEstatico;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirDadosECalcularQRCode;
    procedure AtribuirQRCodeEVerificarDados;
  end;

  { TTestQRCodeDinamico }

  TTestQRCodeDinamico = class(TTestCase)
  private
    fQRStr: String;
    fQRDinamico: TACBrPIXQRCodeDinamico;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirDadosECalcularQRCode;
    procedure AtribuirQRCodeEVerificarDados;
  end;

  { TTestCobrancaImediataExemplo1 }

  TTestCobrancaImediataExemplo1 = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCob: TACBrPIXCobSolicitada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestCobrancaImediataComSaquePIX }

  TTestCobrancaImediataComSaquePIX = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCob: TACBrPIXCobSolicitada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestCobrancaImediataComSaquePIX1 }

  TTestCobrancaImediataComSaquePIX1 = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCob: TACBrPIXCobSolicitada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestCobrancaImediataComSaquePIX2 }

  TTestCobrancaImediataComSaquePIX2 = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCob: TACBrPIXCobSolicitada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestCobrancaImediataComSaquePIX3 }

  TTestCobrancaImediataComSaquePIX3 = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCob: TACBrPIXCobSolicitada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestCobrancaGerada }

  TTestCobrancaGerada = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCobGerada: TACBrPIXCobGerada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestProblema }

  TTestProblema = class(TTestCase)
  private
    fJSON: String;
    fACBrPixProblema: TACBrPIXProblema;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestPixConsultados }

  TTestPixConsultados = class(TTestCase)
  private
    fJSON: String;
    fACBrPixConsultados: TACBrPIXConsultados;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestCobsConsultadas }

  TTestCobsConsultadas = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCobsConsultadas: TACBrPIXCobsConsultadas;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestCobRevisada }

  TTestCobRevisada = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCobRevisada: TACBrPIXCobRevisada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValoresCobBody3;
    procedure AtribuirLerReatribuirECompararCobBody3;
  end;

  { TTestCobVSolicitada }

  TTestCobVSolicitada = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCobVSolicitada: TACBrPIXCobVSolicitada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValoresCobrancaComVencimento1;
    procedure AtribuirLerReatribuirECompararCobrancaComVencimento1;
  end;

  { TTestCobVGerada }

  TTestCobVGerada = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCobVGerada: TACBrPIXCobVGerada;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValoresCobrancaComVencimento1;
    procedure AtribuirLerReatribuirECompararCobrancaComVencimento1;
  end;

  { TTestCobsVConsultadas }

  TTestCobsVConsultadas = class(TTestCase)
  private
    fJSON: String;
    fACBrPixCobsVConsultadas: TACBrPIXCobsVConsultadas;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestCobVRevisada }

  TTestCobVRevisada = class(TTestCase)
  private
    fACBrPixCobVRevisada: TACBrPIXCobVRevisada;
    fJSON1: String;
    fJSON2: String;
    fJSON3: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValoresRevisaoCobrancaComVencimento1;
    procedure AtribuirLerReatribuirECompararRevisaoCobrancaComVencimento1;
    procedure AtribuirELerValoresRevisaoCobranca2;
    procedure AtribuirLerReatribuirECompararRevisaoCobranca2;
    procedure AtribuirELerValoresRevisaoCobranca3;
    procedure AtribuirLerReatribuirECompararRevisaoCobranca3;
  end;

  { TTestLoteCobVBody }

  TTestLoteCobVBody = class(TTestCase)
  private
    fJSON: String;
    fACBrPixLoteCobVBody: TACBrPIXLoteCobVBody;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestLoteCobVBodyRevisado }

  TTestLoteCobVBodyRevisado = class(TTestCase)
  private
    fJSON: String;
    fACBrPixLoteCobVBodyRevisado: TACBrPIXLoteCobVBodyRevisado;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestLoteCobVConsultado }

  TTestLoteCobVConsultado = class(TTestCase)
  private
    fJSON: String;
    fACBrPixLoteCobVConsultado: TACBrPIXLoteCobVConsultado;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

  { TTestLotesCobVConsultados }

  TTestLotesCobVConsultados = class(TTestCase)
  private
    fJSON: String;
    fACBrPIXLotesCobVConsultados: TACBrPIXLotesCobVConsultados;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtribuirELerValores;
    procedure AtribuirLerReatribuirEComparar;
  end;

implementation

uses
  DateUtils,
  ACBrUtil.Strings;

{ TTestCobrancaImediataComSaquePIX1 }

procedure TTestCobrancaImediataComSaquePIX1.SetUp;
begin
  inherited;
  fACBrPixCob := TACBrPIXCobSolicitada.Create('');

  // Nota, os Fontes dessa Unit estão em CP1252, por isso usamos ACBrStr()
  fJSON := ACBrStr(
           '{'+
            '"devedor": {'+
              '"cnpj": "12345678000195",'+
              '"nome": "Empresa de Serviços SA"'+
            '},'+
            '"valor": {'+
              '"original": "0.00",'+
              '"retirada": {'+
                '"saque": {'+
                  '"valor": "5.00",'+
                  '"modalidadeAgente": "AGPSS",'+
                  '"prestadorDoServicoDeSaque": "12345678"'+
                '}'+
              '}'+
            '},'+
            '"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906"'+
           '}');
end;

procedure TTestCobrancaImediataComSaquePIX1.TearDown;
begin
  fACBrPixCob.Free;
  inherited TearDown;
end;

procedure TTestCobrancaImediataComSaquePIX1.AtribuirELerValores;
begin
  fACBrPixCob.AsJSON := fJSON;
  CheckEquals(fACBrPixCob.devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCob.devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCob.valor.original, 0);
  CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCob.valor.retirada.saque.valor, 5);
  CheckEquals(fACBrPixCob.valor.retirada.saque.modalidadeAlteracao, False);
  CheckTrue(fACBrPixCob.valor.retirada.saque.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixCob.valor.retirada.saque.prestadorDoServicoDeSaque, 12345678);
  CheckEquals(fACBrPixCob.chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');
end;

procedure TTestCobrancaImediataComSaquePIX1.AtribuirLerReatribuirEComparar;
var
  pc: TACBrPIXCobSolicitada;
  s: String;
begin
  fACBrPixCob.AsJSON := fJSON;
  s := fACBrPixCob.AsJSON;
  pc := TACBrPIXCobSolicitada.Create('');
  try
    pc.AsJSON := s;
    CheckEquals(fACBrPixCob.devedor.cnpj, pc.devedor.cnpj);
    CheckEquals(fACBrPixCob.devedor.nome, pc.devedor.nome);
    CheckEquals(fACBrPixCob.valor.original, pc.valor.original);
    CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, pc.valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCob.valor.retirada.saque.valor, pc.valor.retirada.saque.valor);
    CheckEquals(fACBrPixCob.valor.retirada.saque.modalidadeAlteracao, pc.valor.retirada.saque.modalidadeAlteracao);
    CheckTrue(fACBrPixCob.valor.retirada.saque.modalidadeAgente = pc.valor.retirada.saque.modalidadeAgente);
    CheckEquals(fACBrPixCob.valor.retirada.saque.prestadorDoServicoDeSaque, pc.valor.retirada.saque.prestadorDoServicoDeSaque);
    CheckEquals(fACBrPixCob.chave, pc.chave);
  finally
    pc.Free;
  end;
end;

{ TTestQRCodeEstatico }

procedure TTestQRCodeEstatico.SetUp;
begin
  inherited SetUp;
  fQREstatico := TACBrPIXQRCodeEstatico.Create;
  fQRStr := '00020126580014br.gov.bcb.pix'+
            '0136123e4567-e12b-12d1-a456-42665544000052040000'+
            '53039865802BR5913Fulano de Tal6008BRASILIA62070503***63041D3D';
end;

procedure TTestQRCodeEstatico.TearDown;
begin
  fQREstatico.Free;
  inherited TearDown;
end;

procedure TTestQRCodeEstatico.AtribuirDadosECalcularQRCode;
begin
  fQREstatico.PixKey := '123e4567-e12b-12d1-a456-426655440000';
  fQREstatico.MerchantName := 'Fulano de Tal';
  fQREstatico.MerchantCity := 'BRASILIA';

  CheckEquals(fQREstatico.AsString, fQRStr);
end;

procedure TTestQRCodeEstatico.AtribuirQRCodeEVerificarDados;
begin
  fQREstatico.AsString := fQRStr;
  CheckEquals(fQREstatico.PixKey, '123e4567-e12b-12d1-a456-426655440000');
  CheckEquals(fQREstatico.MerchantName, 'Fulano de Tal');
  CheckEquals(fQREstatico.MerchantCity, 'BRASILIA');
  CheckEquals(fQREstatico.AsString, fQRStr);
end;

{ TTestQRCodeDinamico }

procedure TTestQRCodeDinamico.SetUp;
begin
  inherited SetUp;
  fQRDinamico := TACBrPIXQRCodeDinamico.Create;
  fQRStr := '000201'+
            '010212'+
            '2687'+
              '0014br.gov.bcb.pix'+
              '2565qrcodepix-h.bb.com.br/pix/v2/d9b97f73-b48b-43f6-be7e-f0594fb275a6'+
            '52040000'+
            '5303986'+
            '5802BR'+
            '5920ALAN GUIACHERO BUENO'+
            '6008BRASILIA'+
            '6207'+
              '0503***'+
            '630464FF';
end;

procedure TTestQRCodeDinamico.TearDown;
begin
  fQRDinamico.Free;
  inherited TearDown;
end;

procedure TTestQRCodeDinamico.AtribuirDadosECalcularQRCode;
begin
  fQRDinamico.PointOfInformationMethod := 12;
  fQRDinamico.URL := 'qrcodepix-h.bb.com.br/pix/v2/d9b97f73-b48b-43f6-be7e-f0594fb275a6';
  fQRDinamico.MerchantName := 'ALAN GUIACHERO BUENO';
  fQRDinamico.MerchantCity := 'BRASILIA';

  CheckEquals(fQRDinamico.AsString, fQRStr);
end;

procedure TTestQRCodeDinamico.AtribuirQRCodeEVerificarDados;
begin
  fQRDinamico.AsString := fQRStr;
  CheckEquals(fQRDinamico.PointOfInformationMethod, 12);
  CheckEquals(fQRDinamico.URL, 'qrcodepix-h.bb.com.br/pix/v2/d9b97f73-b48b-43f6-be7e-f0594fb275a6');
  CheckEquals(fQRDinamico.MerchantName, 'ALAN GUIACHERO BUENO');
  CheckEquals(fQRDinamico.MerchantCity, 'BRASILIA');
  CheckEquals(fQRDinamico.AsString, fQRStr);
end;

{ TTestCobrancaImediataExemplo1 }

procedure TTestCobrancaImediataExemplo1.SetUp;
begin
  inherited;
  fACBrPixCob := TACBrPIXCobSolicitada.Create('');

  // Nota, os Fontes dessa Unit estão em CP1252, por isso usamos ACBrStr()
  fJSON := ACBrStr(
        '{'+
          '"calendario": {'+
            '"expiracao": 3600'+
          '},'+
          '"devedor": {'+
            '"cnpj": "12345678000195",'+
            '"nome": "Empresa de Serviços SA"'+
          '},'+
          '"valor": {'+
            '"original": "37.00",'+
            '"modalidadeAlteracao": 1'+
          '},'+
          '"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906",'+
          '"solicitacaoPagador": "Serviço realizado.",'+
          '"infoAdicionais": ['+
            '{'+
              '"nome": "Campo 1",'+
              '"valor": "Informação Adicional1 do PSP-Recebedor"'+
            '},'+
            '{'+
              '"nome": "Campo 2",'+
              '"valor": "Informação Adicional2 do PSP-Recebedor"'+
            '}'+
          ']'+
        '}');
end;

procedure TTestCobrancaImediataExemplo1.TearDown;
begin
  fACBrPixCob.Free;
  inherited TearDown;
end;

procedure TTestCobrancaImediataExemplo1.AtribuirELerValores;
begin
  fACBrPixCob.AsJSON := fJSON;
  CheckEquals(fACBrPixCob.calendario.expiracao, 3600);
  CheckEquals(fACBrPixCob.devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCob.devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCob.valor.original, 37);
  CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, True);
  CheckEquals(fACBrPixCob.chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');
  CheckEquals(fACBrPixCob.solicitacaoPagador, ACBrStr('Serviço realizado.'));
  CheckEquals(fACBrPixCob.infoAdicionais[0].nome, 'Campo 1');
  CheckEquals(fACBrPixCob.infoAdicionais[0].valor, ACBrStr('Informação Adicional1 do PSP-Recebedor'));
  CheckEquals(fACBrPixCob.infoAdicionais[1].nome, 'Campo 2');
  CheckEquals(fACBrPixCob.infoAdicionais[1].valor, ACBrStr('Informação Adicional2 do PSP-Recebedor'));
end;

procedure TTestCobrancaImediataExemplo1.AtribuirLerReatribuirEComparar;
var
  pc: TACBrPIXCobSolicitada;
  s: String;
begin
  fACBrPixCob.AsJSON := fJSON;
  s := fACBrPixCob.AsJSON;
  pc := TACBrPIXCobSolicitada.Create('');
  try
    // pc.AsJSON := s;
    pc.Assign(fACBrPixCob);
    CheckEquals(fACBrPixCob.calendario.expiracao, pc.calendario.expiracao);
    CheckEquals(fACBrPixCob.devedor.cnpj, pc.devedor.cnpj);
    CheckEquals(fACBrPixCob.devedor.nome, pc.devedor.nome);
    CheckEquals(fACBrPixCob.valor.original, pc.valor.original);
    CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, pc.valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCob.chave, pc.chave);
    CheckEquals(fACBrPixCob.solicitacaoPagador, pc.solicitacaoPagador);
    CheckEquals(fACBrPixCob.infoAdicionais[0].nome, pc.infoAdicionais[0].nome);
    CheckEquals(fACBrPixCob.infoAdicionais[0].valor, pc.infoAdicionais[0].valor);
    CheckEquals(fACBrPixCob.infoAdicionais[1].nome, pc.infoAdicionais[1].nome);
    CheckEquals(fACBrPixCob.infoAdicionais[1].valor, pc.infoAdicionais[1].valor);
  finally
    pc.Free;
  end;
end;

{ TTestCobrancaImediataComSaquePIX }

procedure TTestCobrancaImediataComSaquePIX.SetUp;
begin
  inherited;
  fACBrPixCob := TACBrPIXCobSolicitada.Create('');

  // Nota, os Fontes dessa Unit estão em CP1252, por isso usamos ACBrStr()
  fJSON := ACBrStr(
           '{'+
            '"devedor": {'+
              '"cnpj": "12345678000195",'+
              '"nome": "Empresa de Serviços SA"'+
            '},'+
            '"valor": {'+
              '"original": "0.00",'+
              '"modalidadeAlteracao": 0,'+
              '"retirada": {'+
                '"saque": {'+
                  '"valor": "5.00",'+
                  '"modalidadeAlteracao": 0,'+
                  '"modalidadeAgente": "AGPSS",'+
                  '"prestadorDoServicoDeSaque": "12345678"'+
                '}'+
              '}'+
            '},'+
            '"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906"'+
           '}');
end;

procedure TTestCobrancaImediataComSaquePIX.TearDown;
begin
  fACBrPixCob.Free;
  inherited TearDown;
end;

procedure TTestCobrancaImediataComSaquePIX.AtribuirELerValores;
begin
  fACBrPixCob.AsJSON := fJSON;
  CheckEquals(fACBrPixCob.devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCob.devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCob.valor.original, 0);
  CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCob.valor.retirada.saque.valor, 5);
  CheckEquals(fACBrPixCob.valor.retirada.saque.modalidadeAlteracao, False);
  CheckTrue(fACBrPixCob.valor.retirada.saque.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixCob.valor.retirada.saque.prestadorDoServicoDeSaque, 12345678);
  CheckEquals(fACBrPixCob.chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');
end;

procedure TTestCobrancaImediataComSaquePIX.AtribuirLerReatribuirEComparar;
var
  pc: TACBrPIXCobSolicitada;
  s: String;
begin
  fACBrPixCob.AsJSON := fJSON;
  s := fACBrPixCob.AsJSON;
  pc := TACBrPIXCobSolicitada.Create('');
  try
    pc.AsJSON := s;
    CheckEquals(fACBrPixCob.devedor.cnpj, pc.devedor.cnpj);
    CheckEquals(fACBrPixCob.devedor.nome, pc.devedor.nome);
    CheckEquals(fACBrPixCob.valor.original, pc.valor.original);
    CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, pc.valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCob.valor.retirada.saque.valor, pc.valor.retirada.saque.valor);
    CheckEquals(fACBrPixCob.valor.retirada.saque.modalidadeAlteracao, pc.valor.retirada.saque.modalidadeAlteracao);
    CheckTrue(fACBrPixCob.valor.retirada.saque.modalidadeAgente = pc.valor.retirada.saque.modalidadeAgente);
    CheckEquals(fACBrPixCob.valor.retirada.saque.prestadorDoServicoDeSaque, pc.valor.retirada.saque.prestadorDoServicoDeSaque);
    CheckEquals(fACBrPixCob.chave, pc.chave);
  finally
    pc.Free;
  end;
end;

{ TTestCobrancaImediataComSaquePIX2 }

procedure TTestCobrancaImediataComSaquePIX2.SetUp;
begin
  inherited;
  fACBrPixCob := TACBrPIXCobSolicitada.Create('');

  // Nota, os Fontes dessa Unit estão em CP1252, por isso usamos ACBrStr()
  fJSON := ACBrStr(
           '{'+
            '"devedor": {'+
              '"cnpj": "12345678000195",'+
              '"nome": "Empresa de Serviços SA"'+
            '},'+
            '"valor": {'+
              '"original": "0.00",'+
              '"modalidadeAlteracao": 0,'+
              '"retirada": {'+
                '"saque": {'+
                  '"valor": "20.00",'+
                  '"modalidadeAlteracao": 1,'+
                  '"modalidadeAgente": "AGPSS",'+
                  '"prestadorDoServicoDeSaque": "12345678"'+
                '}'+
              '}'+
            '},'+
            '"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906"'+
           '}');
end;

procedure TTestCobrancaImediataComSaquePIX2.TearDown;
begin
  fACBrPixCob.Free;
  inherited TearDown;
end;

procedure TTestCobrancaImediataComSaquePIX2.AtribuirELerValores;
begin
  fACBrPixCob.AsJSON := fJSON;
  CheckEquals(fACBrPixCob.devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCob.devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCob.valor.original, 0);
  CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCob.valor.retirada.saque.valor, 20);
  CheckEquals(fACBrPixCob.valor.retirada.saque.modalidadeAlteracao, True);
  CheckTrue(fACBrPixCob.valor.retirada.saque.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixCob.valor.retirada.saque.prestadorDoServicoDeSaque, 12345678);
  CheckEquals(fACBrPixCob.chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');
end;

procedure TTestCobrancaImediataComSaquePIX2.AtribuirLerReatribuirEComparar;
var
  pc: TACBrPIXCobSolicitada;
  s: String;
begin
  fACBrPixCob.AsJSON := fJSON;
  s := fACBrPixCob.AsJSON;
  pc := TACBrPIXCobSolicitada.Create('');
  try
    pc.AsJSON := s;
    CheckEquals(fACBrPixCob.devedor.cnpj, pc.devedor.cnpj);
    CheckEquals(fACBrPixCob.devedor.nome, pc.devedor.nome);
    CheckEquals(fACBrPixCob.valor.original, pc.valor.original);
    CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, pc.valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCob.valor.retirada.saque.valor, pc.valor.retirada.saque.valor);
    CheckEquals(fACBrPixCob.valor.retirada.saque.modalidadeAlteracao, pc.valor.retirada.saque.modalidadeAlteracao);
    CheckTrue(fACBrPixCob.valor.retirada.saque.modalidadeAgente = pc.valor.retirada.saque.modalidadeAgente);
    CheckEquals(fACBrPixCob.valor.retirada.saque.prestadorDoServicoDeSaque, pc.valor.retirada.saque.prestadorDoServicoDeSaque);
    CheckEquals(fACBrPixCob.chave, pc.chave);
  finally
    pc.Free;
  end;
end;

{ TTestCobrancaImediataComSaquePIX3 }

procedure TTestCobrancaImediataComSaquePIX3.SetUp;
begin
  inherited;
  fACBrPixCob := TACBrPIXCobSolicitada.Create('');

  // Nota, os Fontes dessa Unit estão em CP1252, por isso usamos ACBrStr()
  fJSON := ACBrStr(
                '{'+
                  '"devedor": {'+
                    '"cnpj": "12345678000195",'+
                    '"nome": "Empresa de Serviços SA"'+
                  '},'+
                  '"valor": {'+
                    '"original": "10.00",'+
                    '"modalidadeAlteracao": 0,'+
                    '"retirada": {'+
                      '"troco": {'+
                        '"valor": "0.00",'+
                        '"modalidadeAlteracao": 1,'+
                        '"modalidadeAgente": "AGPSS",'+
                        '"prestadorDoServicoDeSaque": "12345678"'+
                      '}'+
                    '}'+
                  '},'+
                  '"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906"'+
                '}');
end;

procedure TTestCobrancaImediataComSaquePIX3.TearDown;
begin
  fACBrPixCob.Free;
  inherited TearDown;
end;

procedure TTestCobrancaImediataComSaquePIX3.AtribuirELerValores;
begin
  fACBrPixCob.AsJSON := fJSON;
  CheckEquals(fACBrPixCob.devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCob.devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCob.valor.original, 10);
  CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCob.valor.retirada.troco.valor, 0);
  CheckEquals(fACBrPixCob.valor.retirada.troco.modalidadeAlteracao, True);
  CheckTrue(fACBrPixCob.valor.retirada.troco.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixCob.valor.retirada.troco.prestadorDoServicoDeSaque, 12345678);
  CheckEquals(fACBrPixCob.chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');
end;

procedure TTestCobrancaImediataComSaquePIX3.AtribuirLerReatribuirEComparar;
var
  pc: TACBrPIXCobSolicitada;
  s: String;
begin
  fACBrPixCob.AsJSON := fJSON;
  s := fACBrPixCob.AsJSON;
  pc := TACBrPIXCobSolicitada.Create('');
  try
    pc.AsJSON := s;
    CheckEquals(fACBrPixCob.devedor.cnpj, pc.devedor.cnpj);
    CheckEquals(fACBrPixCob.devedor.nome, pc.devedor.nome);
    CheckEquals(fACBrPixCob.valor.original, pc.valor.original);
    CheckEquals(fACBrPixCob.valor.modalidadeAlteracao, pc.valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCob.valor.retirada.troco.valor, pc.valor.retirada.troco.valor);
    CheckEquals(fACBrPixCob.valor.retirada.troco.modalidadeAlteracao, pc.valor.retirada.troco.modalidadeAlteracao);
    CheckTrue(fACBrPixCob.valor.retirada.troco.modalidadeAgente = pc.valor.retirada.troco.modalidadeAgente);
    CheckEquals(fACBrPixCob.valor.retirada.troco.prestadorDoServicoDeSaque, pc.valor.retirada.troco.prestadorDoServicoDeSaque);
    CheckEquals(fACBrPixCob.chave, pc.chave);
  finally
    pc.Free;
  end;
end;

{ TTestCobrancaGerada }

procedure TTestCobrancaGerada.SetUp;
begin
  inherited SetUp;
  fACBrPixCobGerada := TACBrPIXCobGerada.Create('');
  fJSON := ACBrStr(
  '{'+
    '"calendario": {'+
      '"criacao": "2022-06-11T00:09:30.82-04:00",'+
      '"expiracao": 3600'+
    '},'+
    '"txid": "33beb661beda44a8928fef47dbeb2dc5",'+
    '"revisao": 0,'+
    '"loc": {'+
      '"id": 1004,'+
      '"location": "pix.example.com/qr/7faa6893c4e64893a503baf0d40af213",'+
      '"tipoCob": "cob"'+
    '},'+
    '"location": "pix.example.com/qr/7faa6893c4e64893a503baf0d40af213",'+
    '"status": "ATIVA",'+
    '"devedor": {'+
      '"cnpj": "12345678000195",'+
      '"nome": "Empresa de Serviços SA"'+
    '},'+
    '"valor": {'+
      '"original": "0.00",'+
      '"modalidadeAlteracao": 0,'+
      '"retirada": {'+
        '"saque": {'+
          '"valor": "5.00",'+
          '"modalidadeAlteracao": 0,'+
          '"modalidadeAgente": "AGPSS",'+
          '"prestadorDoServicoDeSaque": "12345678"'+
        '}'+
      '}'+
    '},'+
    '"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906"'+
  '}' );
end;

procedure TTestCobrancaGerada.TearDown;
begin
  fACBrPixCobGerada.Free;
  inherited TearDown;
end;

procedure TTestCobrancaGerada.AtribuirELerValores;
begin
  fACBrPixCobGerada.AsJSON := fJSON;
  CheckEquals(fACBrPixCobGerada.calendario.criacao, EncodeDateTime(2022,06,11,00,09,30,82));
  CheckEquals(fACBrPixCobGerada.calendario.criacao_Bias, 240);
  CheckEquals(fACBrPixCobGerada.calendario.expiracao, 3600);
  CheckEquals(fACBrPixCobGerada.txId, '33beb661beda44a8928fef47dbeb2dc5');
  CheckEquals(fACBrPixCobGerada.revisao, 0);
  CheckEquals(fACBrPixCobGerada.loc.id, 1004);
  CheckEquals(fACBrPixCobGerada.loc.location, 'pix.example.com/qr/7faa6893c4e64893a503baf0d40af213');
  CheckTrue(fACBrPixCobGerada.loc.tipoCob = tcoCob);
  CheckEquals(fACBrPixCobGerada.location, 'pix.example.com/qr/7faa6893c4e64893a503baf0d40af213');
  CheckTrue(fACBrPixCobGerada.status = stcATIVA);
  CheckEquals(fACBrPixCobGerada.devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCobGerada.devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCobGerada.valor.original, 0);
  CheckEquals(fACBrPixCobGerada.valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCobGerada.valor.retirada.saque.valor, 5);
  CheckEquals(fACBrPixCobGerada.valor.retirada.saque.modalidadeAlteracao, False);
  CheckTrue(fACBrPixCobGerada.valor.retirada.saque.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixCobGerada.valor.retirada.saque.prestadorDoServicoDeSaque, 12345678);
  CheckEquals(fACBrPixCobGerada.chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');
end;

procedure TTestCobrancaGerada.AtribuirLerReatribuirEComparar;
var
  cg: TACBrPIXCobGerada;
  s: String;
begin
  fACBrPixCobGerada.AsJSON := fJSON;
  s := fACBrPixCobGerada.AsJSON;
  cg := TACBrPIXCobGerada.Create('');
  try
    cg.AsJSON := s;
    CheckEquals(fACBrPixCobGerada.calendario.criacao, cg.calendario.criacao);
    CheckEquals(fACBrPixCobGerada.calendario.criacao_Bias, cg.calendario.criacao_Bias);
    CheckEquals(fACBrPixCobGerada.calendario.expiracao, cg.calendario.expiracao);
    CheckEquals(fACBrPixCobGerada.txId, cg.txId);
    CheckEquals(fACBrPixCobGerada.revisao, cg.revisao);
    CheckEquals(fACBrPixCobGerada.loc.id, cg.loc.id);
    CheckEquals(fACBrPixCobGerada.loc.location, cg.loc.location);
    CheckTrue(fACBrPixCobGerada.loc.tipoCob = cg.loc.tipoCob);
    CheckEquals(fACBrPixCobGerada.location, cg.location);
    CheckTrue(fACBrPixCobGerada.status = cg.status);
    CheckEquals(fACBrPixCobGerada.devedor.cnpj, cg.devedor.cnpj);
    CheckEquals(fACBrPixCobGerada.devedor.nome, cg.devedor.nome);
    CheckEquals(fACBrPixCobGerada.valor.original, cg.valor.original);
    CheckEquals(fACBrPixCobGerada.valor.modalidadeAlteracao, cg.valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCobGerada.valor.retirada.saque.valor, cg.valor.retirada.saque.valor);
    CheckEquals(fACBrPixCobGerada.valor.retirada.saque.modalidadeAlteracao, cg.valor.retirada.saque.modalidadeAlteracao);
    CheckTrue(fACBrPixCobGerada.valor.retirada.saque.modalidadeAgente = cg.valor.retirada.saque.modalidadeAgente);
    CheckEquals(fACBrPixCobGerada.valor.retirada.saque.prestadorDoServicoDeSaque, cg.valor.retirada.saque.prestadorDoServicoDeSaque);
    CheckEquals(fACBrPixCobGerada.chave, cg.chave);
  finally
    cg.Free;
  end;
end;

{ TTestProblema }

procedure TTestProblema.SetUp;
begin
  inherited SetUp;
  fACBrPixProblema := TACBrPIXProblema.Create('');
  fJSON :=  ACBrStr(
      '{'+
        '"type": "https://pix.bcb.gov.br/api/v2/error/CobOperacaoInvalida",'+
        '"title": "Cobrança inválida.",'+
        '"status": 400,'+
        '"detail": "A requisição que busca alterar ou criar uma cobrança para pagamento imediato não respeita o _schema_ ou está semanticamente errada.",'+
        '"violacoes": ['+
          '{'+
            '"razao": "O campo cob.valor.original não respeita o _schema_.",'+
            '"propriedade": "cob.valor.original"'+
          '}'+
        ']'+
      '}');
end;

procedure TTestProblema.TearDown;
begin
  fACBrPixProblema.Free;
  inherited TearDown;
end;

procedure TTestProblema.AtribuirELerValores;
begin
  fACBrPixProblema.AsJSON := fJSON;
  CheckEquals(fACBrPixProblema.type_uri, 'https://pix.bcb.gov.br/api/v2/error/CobOperacaoInvalida');
  CheckEquals(fACBrPixProblema.title, ACBrStr('Cobrança inválida.'));
  CheckEquals(fACBrPixProblema.status, 400);
  CheckEquals(fACBrPixProblema.detail, ACBrStr('A requisição que busca alterar ou criar uma cobrança para pagamento imediato não respeita o _schema_ ou está semanticamente errada.'));
  CheckEquals(fACBrPixProblema.violacoes[0].razao, ACBrStr('O campo cob.valor.original não respeita o _schema_.'));
  CheckEquals(fACBrPixProblema.violacoes[0].propriedade, 'cob.valor.original');
end;

procedure TTestProblema.AtribuirLerReatribuirEComparar;
var
  pb: TACBrPIXProblema;
  s: String;
begin
  fACBrPixProblema.AsJSON := fJSON;
  s := fACBrPixProblema.AsJSON;
  pb := TACBrPIXProblema.Create('');
  try
    pb.AsJSON := s;
    CheckEquals(fACBrPixProblema.type_uri, pb.type_uri);
    CheckEquals(fACBrPixProblema.title, pb.title);
    CheckEquals(fACBrPixProblema.status, pb.status);
    CheckEquals(fACBrPixProblema.detail, pb.detail);
    CheckEquals(fACBrPixProblema.violacoes[0].razao, pb.violacoes[0].razao);
    CheckEquals(fACBrPixProblema.violacoes[0].propriedade, pb.violacoes[0].propriedade);
  finally
    pb.Free;
  end;
end;

{ TTestPixConsultados }

procedure TTestPixConsultados.SetUp;
begin
  inherited SetUp;
  fJSON := ACBrStr(
  '{'+
    '"parametros": {'+
      '"inicio": "2020-04-01T00:00:00Z",'+
      '"fim": "2020-04-01T23:59:59Z",'+
      '"paginacao": {'+
        '"paginaAtual": 0,'+
  	'"itensPorPagina": 100,'+
  	'"quantidadeDePaginas": 1,'+
  	'"quantidadeTotalDeItens": 2'+
      '}'+
    '},'+
    '"pix": ['+
      '{'+
        '"endToEndId": "E12345678202009091221abcdef12345",'+
  	'"txid": "cd1fe328c875481285a6f233ae41b662",'+
  	'"valor": "100.00",'+
  	'"horario": "2020-09-10T13:03:33.902Z",'+
  	'"infoPagador": "Reforma da casa",'+
  	'"devolucoes": ['+
	  '{'+
  	    '"id": "000AAA111",'+
  	    '"rtrId": "D12345678202009091000abcde123456",'+
  	    '"valor": "11.00",'+
  	    '"horario": {'+
  	      '"solicitacao": "2020-09-10T13:03:33.902Z"'+
  	    '},'+
  	    '"status": "EM_PROCESSAMENTO"'+
  	  '}'+
        ']'+
      '},'+
      '{'+
        '"endToEndId": "E12345678202009091221ghijk78901234",'+
  	'"txid": "5b933948f3224266b1050ac54319e775",'+
  	'"valor": "200.00",'+
  	'"horario": "2020-09-10T13:03:33.902Z",'+
  	'"infoPagador": "Revisão do carro"'+
      '},'+
      '{'+
        '"endToEndId": "E88631478202009091221ghijk78901234",'+
  	'"txid": "82433415910c47e5adb6ac3527cca160",'+
  	'"valor": "200.00",'+
  	'"componentesValor": {'+
  	  '"original": {'+
  	    '"valor": "180.00"'+
  	  '},'+
  	  '"saque": {'+
  	    '"valor": "20.00",'+
            '"modalidadeAgente": "AGPSS",'+
            '"prestadorDeServicoDeSaque": "12345678"'+
  	  '}'+
  	'},'+
  	'"horario": "2020-09-10T13:03:33.902Z",'+
  	'"infoPagador": "Saque Pix"'+
      '}'+
    ']'+
  '}');
  fACBrPixConsultados := TACBrPIXConsultados.Create('');
end;

procedure TTestPixConsultados.TearDown;
begin
  fACBrPixConsultados.Free;
  inherited TearDown;
end;

procedure TTestPixConsultados.AtribuirELerValores;
begin
  fACBrPixConsultados.AsJSON := fJSON;
  CheckEquals(fACBrPixConsultados.parametros.inicio, EncodeDate(2020,04,01));
  CheckEquals(fACBrPixConsultados.parametros.fim, EncodeDateTime(2020,04,01,23,59,59,0));
  CheckEquals(fACBrPixConsultados.parametros.paginacao.paginaAtual, 0);
  CheckEquals(fACBrPixConsultados.parametros.paginacao.itensPorPagina, 100);
  CheckEquals(fACBrPixConsultados.parametros.paginacao.quantidadeDePaginas, 1);
  CheckEquals(fACBrPixConsultados.parametros.paginacao.quantidadeTotalDeItens, 2);
  CheckEquals(fACBrPixConsultados.pix[0].endToEndId, 'E12345678202009091221abcdef12345');
  CheckEquals(fACBrPixConsultados.pix[0].txid, 'cd1fe328c875481285a6f233ae41b662');
  CheckEquals(fACBrPixConsultados.pix[0].valor, 100);
  CheckEquals(fACBrPixConsultados.pix[0].horario, EncodeDateTime(2020,09,10,13,03,33,902));
  CheckEquals(fACBrPixConsultados.pix[0].infoPagador, 'Reforma da casa');
  CheckEquals(fACBrPixConsultados.pix[0].devolucoes[0].id, '000AAA111');
  CheckEquals(fACBrPixConsultados.pix[0].devolucoes[0].rtrId, 'D12345678202009091000abcde123456');
  CheckEquals(fACBrPixConsultados.pix[0].devolucoes[0].valor, 11);
  CheckEquals(fACBrPixConsultados.pix[0].devolucoes[0].horario.solicitacao, EncodeDateTime(2020,09,10,13,03,33,902));
  CheckTrue(fACBrPixConsultados.pix[0].devolucoes[0].status = stdEM_PROCESSAMENTO);
  CheckEquals(fACBrPixConsultados.pix[1].endToEndId, 'E12345678202009091221ghijk78901234');
  CheckEquals(fACBrPixConsultados.pix[1].txid, '5b933948f3224266b1050ac54319e775');
  CheckEquals(fACBrPixConsultados.pix[1].valor, 200);
  CheckEquals(fACBrPixConsultados.pix[1].horario, EncodeDateTime(2020,09,10,13,03,33,902));
  CheckEquals(fACBrPixConsultados.pix[1].infoPagador, ACBrStr('Revisão do carro'));
  CheckEquals(fACBrPixConsultados.pix[2].endToEndId, 'E88631478202009091221ghijk78901234');
  CheckEquals(fACBrPixConsultados.pix[2].txid, '82433415910c47e5adb6ac3527cca160');
  CheckEquals(fACBrPixConsultados.pix[2].valor, 200);
  CheckEquals(fACBrPixConsultados.pix[2].horario, EncodeDateTime(2020,09,10,13,03,33,902));
  CheckEquals(fACBrPixConsultados.pix[2].infoPagador, 'Saque Pix');
  CheckEquals(fACBrPixConsultados.pix[2].componentesValor.original.valor, 180);
  CheckEquals(fACBrPixConsultados.pix[2].componentesValor.saque.valor, 20);
  CheckTrue(fACBrPixConsultados.pix[2].componentesValor.saque.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixConsultados.pix[2].componentesValor.saque.prestadorDoServicoDeSaque, 12345678);
end;

procedure TTestPixConsultados.AtribuirLerReatribuirEComparar;
var
  pc: TACBrPIXConsultados;
  s: String;
begin
  fACBrPixConsultados.AsJSON := fJSON;
  s := fACBrPixConsultados.AsJSON;
  pc := TACBrPIXConsultados.Create('');
  try
    pc.AsJSON := s;
    CheckEquals(fACBrPixConsultados.parametros.inicio, pc.parametros.inicio);
    CheckEquals(fACBrPixConsultados.parametros.fim, pc.parametros.fim);
    CheckEquals(fACBrPixConsultados.parametros.paginacao.paginaAtual, pc.parametros.paginacao.paginaAtual);
    CheckEquals(fACBrPixConsultados.parametros.paginacao.itensPorPagina, pc.parametros.paginacao.itensPorPagina);
    CheckEquals(fACBrPixConsultados.parametros.paginacao.quantidadeDePaginas, pc.parametros.paginacao.quantidadeDePaginas);
    CheckEquals(fACBrPixConsultados.parametros.paginacao.quantidadeTotalDeItens, pc.parametros.paginacao.quantidadeTotalDeItens);
    CheckEquals(fACBrPixConsultados.pix[0].endToEndId, pc.pix[0].endToEndId);
    CheckEquals(fACBrPixConsultados.pix[0].txid, pc.pix[0].txid);
    CheckEquals(fACBrPixConsultados.pix[0].valor, pc.pix[0].valor);
    CheckEquals(fACBrPixConsultados.pix[0].horario, pc.pix[0].horario);
    CheckEquals(fACBrPixConsultados.pix[0].infoPagador, pc.pix[0].infoPagador);
    CheckEquals(fACBrPixConsultados.pix[0].devolucoes[0].id, pc.pix[0].devolucoes[0].id);
    CheckEquals(fACBrPixConsultados.pix[0].devolucoes[0].rtrId, pc.pix[0].devolucoes[0].rtrId);
    CheckEquals(fACBrPixConsultados.pix[0].devolucoes[0].valor, pc.pix[0].devolucoes[0].valor);
    CheckEquals(fACBrPixConsultados.pix[0].devolucoes[0].horario.solicitacao, pc.pix[0].devolucoes[0].horario.solicitacao);
    CheckTrue(fACBrPixConsultados.pix[0].devolucoes[0].status = pc.pix[0].devolucoes[0].status);
    CheckEquals(fACBrPixConsultados.pix[1].endToEndId, pc.pix[1].endToEndId);
    CheckEquals(fACBrPixConsultados.pix[1].txid, pc.pix[1].txid);
    CheckEquals(fACBrPixConsultados.pix[1].valor, pc.pix[1].valor);
    CheckEquals(fACBrPixConsultados.pix[1].horario, pc.pix[1].horario);
    CheckEquals(fACBrPixConsultados.pix[1].infoPagador, pc.pix[1].infoPagador);
    CheckEquals(fACBrPixConsultados.pix[2].endToEndId, pc.pix[2].endToEndId);
    CheckEquals(fACBrPixConsultados.pix[2].txid, pc.pix[2].txid);
    CheckEquals(fACBrPixConsultados.pix[2].valor, pc.pix[2].valor);
    CheckEquals(fACBrPixConsultados.pix[2].horario, pc.pix[2].horario);
    CheckEquals(fACBrPixConsultados.pix[2].infoPagador, pc.pix[2].infoPagador);
    CheckEquals(fACBrPixConsultados.pix[2].componentesValor.original.valor, pc.pix[2].componentesValor.original.valor);
    CheckEquals(fACBrPixConsultados.pix[2].componentesValor.saque.valor, pc.pix[2].componentesValor.saque.valor);
    CheckTrue(fACBrPixConsultados.pix[2].componentesValor.saque.modalidadeAgente = pc.pix[2].componentesValor.saque.modalidadeAgente);
    CheckEquals(fACBrPixConsultados.pix[2].componentesValor.saque.prestadorDoServicoDeSaque, pc.pix[2].componentesValor.saque.prestadorDoServicoDeSaque);
  finally
    pc.Free;
  end;
end;

{ TTestCobsConsultadas }

procedure TTestCobsConsultadas.SetUp;
begin
  inherited SetUp;
  fACBrPixCobsConsultadas := TACBrPIXCobsConsultadas.Create('');
  fJSON := ACBrStr(
  '{'+
  	'"parametros": {'+
  		'"inicio": "2020-04-01T00:00:00Z",'+
  		'"fim": "2020-04-02T10:00:00Z",'+
  		'"paginacao": {'+
  			'"paginaAtual": 0,'+
  			'"itensPorPagina": 100,'+
  			'"quantidadeDePaginas": 1,'+
  			'"quantidadeTotalDeItens": 2'+
  		'}'+
  	'},'+
  	'"cobs": ['+
  		'{'+
  			'"calendario": {'+
  				'"criacao": "2020-09-09T20:15:00.358Z",'+
  				'"expiracao": 3600'+
  			'},'+
  			'"txid": "7978c0c97ea847e78e8849634473c1f1",'+
  			'"revisao": 0,'+
  			'"loc": {'+
  				'"id": 789,'+
  				'"location": "pix.example.com/qr/9d36b84fc70b478fb95c12729b90ca25",'+
  				'"tipoCob": "cob"'+
  			'},'+
  			'"location": "pix.example.com/qr/9d36b84fc70b478fb95c12729b90ca25",'+
  			'"status": "ATIVA",'+
  			'"devedor": {'+
  				'"cnpj": "12345678000195",'+
  				'"nome": "Empresa de Serviços SA"'+
  			'},'+
  			'"valor": {'+
  				'"original": "37.00",'+
  				'"modalidadeAlteracao": 1'+
  			'},'+
  			'"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906",'+
  			'"solicitacaoPagador": "Serviço realizado.",'+
  			'"infoAdicionais": ['+
  				'{'+
  					'"nome": "Campo 1",'+
  					'"valor": "Informação Adicional1 do PSP-Recebedor"'+
  				'},'+
  				'{'+
  					'"nome": "Campo 2",'+
  					'"valor": "Informação Adicional2 do PSP-Recebedor"'+
  				'}'+
  			']'+
  		'},'+
  		'{'+
  			'"calendario": {'+
  				'"criacao": "2020-09-09T20:15:00.358-04:00",'+
  				'"expiracao": 3600'+
  			'},'+
  			'"txid": "655dfdb1a4514b8fbb58254b958913fb",'+
  			'"revisao": 1,'+
  			'"loc": {'+
  				'"id": 567,'+
  				'"location": "pix.example.com/qr/1dd7f893a58e417287028dc33e21a403"'+
  			'},'+
  			'"location": "pix.example.com/qr/1dd7f893a58e417287028dc33e21a403",'+
  			'"status": "CONCLUIDA",'+
  			'"devedor": {'+
  				'"cnpj": "12345678000195",'+
  				'"nome": "Empresa de Serviços SA"'+
  			'},'+
  			'"valor": {'+
  				'"original": "100.00",'+
  				'"modalidadeAlteracao": 0'+
  			'},'+
  			'"chave": "40a0932d-1918-4eee-845d-35a2da1690dc",'+
  			'"solicitacaoPagador": "Informar cartão fidelidade",'+
  			'"pix": ['+
  				'{'+
  					'"endToEndId": "E12345678202009091221kkkkkkkkkkk",'+
  					'"txid": "655dfdb1a4514b8fbb58254b958913fb",'+
  					'"valor": "110.00",'+
  					'"horario": "2020-09-09T20:15:00.358Z",'+
  					'"infoPagador": "0123456789",'+
  					'"devolucoes": ['+
  						'{'+
  							'"id": "123ABC",'+
  							'"rtrId": "Dxxxxxxxx202009091221kkkkkkkkkkk",'+
  							'"valor": "10.00",'+
  							'"horario": {'+
  								'"solicitacao": "2020-09-09T20:15:00.358Z"'+
  							'},'+
  							'"status": "EM_PROCESSAMENTO"'+
  						'}'+
  					']'+
  				'}'+
  			']'+
  		'},'+
  		'{'+
  			'"calendario": {'+
  				'"criacao": "2020-09-09T20:15:00.358-03:00",'+
  				'"expiracao": 3600'+
  			'},'+
  			'"txid": "33beb661beda44a8928fef47dbeb2dc5",'+
  			'"revisao": 0,'+
  			'"loc": {'+
  				'"id": 1004,'+
  				'"location": "pix.example.com/qr/7faa6893c4e64893a503baf0d40af213",'+
  				'"tipoCob": "cob"'+
  			'},'+
  			'"location": "pix.example.com/qr/7faa6893c4e64893a503baf0d40af213",'+
  			'"status": "ATIVA",'+
  			'"devedor": {'+
  				'"cnpj": "12345678000195",'+
  				'"nome": "Empresa de Serviços SA"'+
  			'},'+
  			'"valor": {'+
  				'"original": "0.00",'+
  				'"modalidadeAlteracao": 0,'+
  				'"retirada": {'+
  					'"saque": {'+
  						'"valor": "5.00",'+
  						'"modalidadeAlteracao": 0,'+
  						'"modalidadeAgente": "AGPSS",'+
  						'"prestadorDoServicoDeSaque": "12345678"'+
  					'}'+
  				'}'+
  			'},'+
  			'"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906"'+
  		'},'+
  		'{'+
  			'"calendario": {'+
  				'"criacao": "2020-09-09T20:15:00.358+01:00",'+
  				'"expiracao": 3600'+
  			'},'+
  			'"txid": "33beb661beda44a8928fef47dbeb2dc5",'+
  			'"revisao": 0,'+
  			'"loc": {'+
  				'"id": 1004,'+
  				'"location": "pix.example.com/qr/7faa6893c4e64893a503baf0d40af213",'+
  				'"tipoCob": "cob"'+
  			'},'+
  			'"location": "pix.example.com/qr/7faa6893c4e64893a503baf0d40af213",'+
  			'"status": "ATIVA",'+
  			'"devedor": {'+
  				'"cnpj": "12345678000195",'+
  				'"nome": "Empresa de Serviços SA"'+
  			'},'+
  			'"valor": {'+
  				'"original": "0.00",'+
  				'"modalidadeAlteracao": 0,'+
  				'"retirada": {'+
  					'"saque": {'+
  						'"valor": "20.00",'+
  						'"modalidadeAlteracao": 1,'+
  						'"modalidadeAgente": "AGPSS",'+
  						'"prestadorDoServicoDeSaque": "12345678"'+
  					'}'+
  				'}'+
  			'},'+
  			'"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906"'+
  		'},'+
  		'{'+
  			'"calendario": {'+
  				'"criacao": "2020-09-09T20:15:00.358Z",'+
  				'"expiracao": 3600'+
  			'},'+
  			'"txid": "33beb661beda44a8928fef47dbeb2dc5",'+
  			'"revisao": 0,'+
  			'"loc": {'+
  				'"id": 1004,'+
  				'"location": "pix.example.com/qr/7faa6893c4e64893a503baf0d40af213",'+
  				'"tipoCob": "cob"'+
  			'},'+
  			'"location": "pix.example.com/qr/7faa6893c4e64893a503baf0d40af213",'+
  			'"status": "ATIVA",'+
  			'"devedor": {'+
  				'"cnpj": "12345678000195",'+
  				'"nome": "Empresa de Serviços SA"'+
  			'},'+
  			'"valor": {'+
  				'"original": "10.00",'+
  				'"modalidadeAlteracao": 0,'+
  				'"retirada": {'+
  					'"troco": {'+
  						'"valor": "0.00",'+
  						'"modalidadeAlteracao": 1,'+
  						'"modalidadeAgente": "AGPSS",'+
  						'"prestadorDoServicoDeSaque": "12345678"'+
  					'}'+
  				'}'+
  			'},'+
  			'"chave": "7d9f0335-8dcc-4054-9bf9-0dbd61d36906"'+
  		'}'+
  	']'+
  '}');
end;

procedure TTestCobsConsultadas.TearDown;
begin
  fACBrPixCobsConsultadas.Free;
  inherited TearDown;
end;

procedure TTestCobsConsultadas.AtribuirELerValores;
begin
  fACBrPixCobsConsultadas.AsJSON := fJSON;

  CheckEquals(fACBrPixCobsConsultadas.parametros.inicio, EncodeDate(2020,04,01));
  CheckEquals(fACBrPixCobsConsultadas.parametros.fim, EncodeDateTime(2020,04,02,10,00,00,0));
  CheckEquals(fACBrPixCobsConsultadas.parametros.paginacao.paginaAtual, 0);
  CheckEquals(fACBrPixCobsConsultadas.parametros.paginacao.itensPorPagina, 100);
  CheckEquals(fACBrPixCobsConsultadas.parametros.paginacao.quantidadeDePaginas, 1);
  CheckEquals(fACBrPixCobsConsultadas.parametros.paginacao.quantidadeTotalDeItens, 2);

  CheckEquals(fACBrPixCobsConsultadas.cobs[0].calendario.criacao, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].calendario.criacao_Bias, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].calendario.expiracao, 3600);
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].txId, '7978c0c97ea847e78e8849634473c1f1');
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].revisao, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].loc.id, 789);
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].loc.location, 'pix.example.com/qr/9d36b84fc70b478fb95c12729b90ca25');
  CheckTrue(fACBrPixCobsConsultadas.cobs[0].loc.tipoCob = tcoCob);
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].location, 'pix.example.com/qr/9d36b84fc70b478fb95c12729b90ca25');
  CheckTrue(fACBrPixCobsConsultadas.cobs[0].status = stcATIVA);
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].valor.original, 37);
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].valor.modalidadeAlteracao, True);
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].solicitacaoPagador, ACBrStr('Serviço realizado.'));
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].infoAdicionais[0].nome, 'Campo 1');
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].infoAdicionais[0].valor, ACBrStr('Informação Adicional1 do PSP-Recebedor'));
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].infoAdicionais[1].nome, 'Campo 2');
  CheckEquals(fACBrPixCobsConsultadas.cobs[0].infoAdicionais[1].valor, ACBrStr('Informação Adicional2 do PSP-Recebedor'));

  CheckEquals(fACBrPixCobsConsultadas.cobs[1].calendario.criacao, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].calendario.criacao_Bias, 240);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].calendario.expiracao, 3600);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].txId, '655dfdb1a4514b8fbb58254b958913fb');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].revisao, 1);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].loc.id, 567);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].loc.location, 'pix.example.com/qr/1dd7f893a58e417287028dc33e21a403');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].location, 'pix.example.com/qr/1dd7f893a58e417287028dc33e21a403');
  CheckTrue(fACBrPixCobsConsultadas.cobs[1].status = stcCONCLUIDA);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].valor.original, 100);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].chave, '40a0932d-1918-4eee-845d-35a2da1690dc');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].solicitacaoPagador, ACBrStr('Informar cartão fidelidade'));
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].endToEndId, 'E12345678202009091221kkkkkkkkkkk');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].txid, '655dfdb1a4514b8fbb58254b958913fb');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].valor, 110);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].horario, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].infoPagador, '0123456789');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].id, '123ABC');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].rtrId, 'Dxxxxxxxx202009091221kkkkkkkkkkk');
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].valor, 10);
  CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].horario.solicitacao, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckTrue(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].status = stdEM_PROCESSAMENTO);

  CheckEquals(fACBrPixCobsConsultadas.cobs[2].calendario.criacao, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].calendario.criacao_Bias, 180);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].calendario.expiracao, 3600);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].txId, '33beb661beda44a8928fef47dbeb2dc5');
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].revisao, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].loc.id, 1004);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].loc.location, 'pix.example.com/qr/7faa6893c4e64893a503baf0d40af213');
  CheckTrue(fACBrPixCobsConsultadas.cobs[2].loc.tipoCob = tcoCob);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].location, 'pix.example.com/qr/7faa6893c4e64893a503baf0d40af213');
  CheckTrue(fACBrPixCobsConsultadas.cobs[2].status = stcATIVA);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.original, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.retirada.saque.valor, 5);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.retirada.saque.modalidadeAlteracao, False);
  CheckTrue(fACBrPixCobsConsultadas.cobs[2].valor.retirada.saque.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.retirada.saque.prestadorDoServicoDeSaque, 12345678);
  CheckEquals(fACBrPixCobsConsultadas.cobs[2].chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');

  CheckEquals(fACBrPixCobsConsultadas.cobs[3].calendario.criacao, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].calendario.criacao_Bias, -60);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].calendario.expiracao, 3600);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].txId, '33beb661beda44a8928fef47dbeb2dc5');
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].revisao, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].loc.id, 1004);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].loc.location, 'pix.example.com/qr/7faa6893c4e64893a503baf0d40af213');
  CheckTrue(fACBrPixCobsConsultadas.cobs[3].loc.tipoCob = tcoCob);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].location, 'pix.example.com/qr/7faa6893c4e64893a503baf0d40af213');
  CheckTrue(fACBrPixCobsConsultadas.cobs[3].status = stcATIVA);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.original, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.retirada.saque.valor, 20);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.retirada.saque.modalidadeAlteracao, True);
  CheckTrue(fACBrPixCobsConsultadas.cobs[3].valor.retirada.saque.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.retirada.saque.prestadorDoServicoDeSaque, 12345678);
  CheckEquals(fACBrPixCobsConsultadas.cobs[3].chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');

  CheckEquals(fACBrPixCobsConsultadas.cobs[4].calendario.criacao, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].calendario.criacao_Bias, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].calendario.expiracao, 3600);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].txId, '33beb661beda44a8928fef47dbeb2dc5');
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].revisao, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].loc.id, 1004);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].loc.location, 'pix.example.com/qr/7faa6893c4e64893a503baf0d40af213');
  CheckTrue(fACBrPixCobsConsultadas.cobs[4].loc.tipoCob = tcoCob);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].location, 'pix.example.com/qr/7faa6893c4e64893a503baf0d40af213');
  CheckTrue(fACBrPixCobsConsultadas.cobs[4].status = stcATIVA);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].devedor.cnpj, '12345678000195');
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].devedor.nome, ACBrStr('Empresa de Serviços SA'));
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.original, 10);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.modalidadeAlteracao, False);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.retirada.troco.valor, 0);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.retirada.troco.modalidadeAlteracao, True);
  CheckTrue(fACBrPixCobsConsultadas.cobs[4].valor.retirada.troco.modalidadeAgente = maAGPSS);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.retirada.troco.prestadorDoServicoDeSaque, 12345678);
  CheckEquals(fACBrPixCobsConsultadas.cobs[4].chave, '7d9f0335-8dcc-4054-9bf9-0dbd61d36906');
end;

procedure TTestCobsConsultadas.AtribuirLerReatribuirEComparar;
var
  cc: TACBrPIXCobsConsultadas;
  s: String;
begin
  fACBrPixCobsConsultadas.AsJSON := fJSON;
  s := fACBrPixCobsConsultadas.AsJSON;
  cc := TACBrPIXCobsConsultadas.Create('');
  try
    cc.AsJSON := s;
    CheckEquals(fACBrPixCobsConsultadas.parametros.inicio, cc.parametros.inicio);
    CheckEquals(fACBrPixCobsConsultadas.parametros.fim, cc.parametros.fim);
    CheckEquals(fACBrPixCobsConsultadas.parametros.paginacao.paginaAtual, cc.parametros.paginacao.paginaAtual);
    CheckEquals(fACBrPixCobsConsultadas.parametros.paginacao.itensPorPagina, cc.parametros.paginacao.itensPorPagina);
    CheckEquals(fACBrPixCobsConsultadas.parametros.paginacao.quantidadeDePaginas, cc.parametros.paginacao.quantidadeDePaginas);
    CheckEquals(fACBrPixCobsConsultadas.parametros.paginacao.quantidadeTotalDeItens, cc.parametros.paginacao.quantidadeTotalDeItens);

    CheckEquals(fACBrPixCobsConsultadas.cobs[0].calendario.criacao, cc.cobs[0].calendario.criacao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].calendario.criacao_Bias, cc.cobs[0].calendario.criacao_Bias);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].calendario.expiracao, cc.cobs[0].calendario.expiracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].txId, cc.cobs[0].txId);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].revisao, cc.cobs[0].revisao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].loc.id, cc.cobs[0].loc.id);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].loc.location, cc.cobs[0].loc.location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[0].loc.tipoCob = cc.cobs[0].loc.tipoCob);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].location, cc.cobs[0].location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[0].status = cc.cobs[0].status);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].devedor.cnpj, cc.cobs[0].devedor.cnpj);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].devedor.nome, cc.cobs[0].devedor.nome);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].valor.original, cc.cobs[0].valor.original);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].valor.modalidadeAlteracao, cc.cobs[0].valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].chave, cc.cobs[0].chave);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].solicitacaoPagador, cc.cobs[0].solicitacaoPagador);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].infoAdicionais[0].nome, cc.cobs[0].infoAdicionais[0].nome);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].infoAdicionais[0].valor, cc.cobs[0].infoAdicionais[0].valor);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].infoAdicionais[1].nome, cc.cobs[0].infoAdicionais[1].nome);
    CheckEquals(fACBrPixCobsConsultadas.cobs[0].infoAdicionais[1].valor, cc.cobs[0].infoAdicionais[1].valor);

    CheckEquals(fACBrPixCobsConsultadas.cobs[1].calendario.criacao, cc.cobs[1].calendario.criacao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].calendario.criacao_Bias, cc.cobs[1].calendario.criacao_Bias);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].calendario.expiracao, cc.cobs[1].calendario.expiracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].txId, cc.cobs[1].txId);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].revisao, cc.cobs[1].revisao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].loc.id, cc.cobs[1].loc.id);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].loc.location, cc.cobs[1].loc.location);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].location, cc.cobs[1].location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[1].status = cc.cobs[1].status);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].devedor.cnpj, cc.cobs[1].devedor.cnpj );
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].devedor.nome, cc.cobs[1].devedor.nome );
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].valor.original, cc.cobs[1].valor.original);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].valor.modalidadeAlteracao, cc.cobs[1].valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].chave, cc.cobs[1].chave);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].solicitacaoPagador, cc.cobs[1].solicitacaoPagador);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].endToEndId, cc.cobs[1].pix[0].endToEndId);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].txid, cc.cobs[1].pix[0].txid);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].valor, cc.cobs[1].pix[0].valor);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].horario, cc.cobs[1].pix[0].horario);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].infoPagador, cc.cobs[1].pix[0].infoPagador);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].id, cc.cobs[1].pix[0].devolucoes[0].id);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].rtrId, cc.cobs[1].pix[0].devolucoes[0].rtrId);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].valor, cc.cobs[1].pix[0].devolucoes[0].valor);
    CheckEquals(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].horario.solicitacao, cc.cobs[1].pix[0].devolucoes[0].horario.solicitacao);
    CheckTrue(fACBrPixCobsConsultadas.cobs[1].pix[0].devolucoes[0].status = cc.cobs[1].pix[0].devolucoes[0].status);

    CheckEquals(fACBrPixCobsConsultadas.cobs[2].calendario.criacao, cc.cobs[2].calendario.criacao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].calendario.criacao_Bias, cc.cobs[2].calendario.criacao_Bias);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].calendario.expiracao, cc.cobs[2].calendario.expiracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].txId, cc.cobs[2].txId);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].revisao, cc.cobs[2].revisao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].loc.id, cc.cobs[2].loc.id);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].loc.location, cc.cobs[2].loc.location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[2].loc.tipoCob = cc.cobs[2].loc.tipoCob);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].location, cc.cobs[2].location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[2].status = cc.cobs[2].status);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].devedor.cnpj, cc.cobs[2].devedor.cnpj);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].devedor.nome, cc.cobs[2].devedor.nome);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.original, cc.cobs[2].valor.original);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.modalidadeAlteracao, cc.cobs[2].valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.retirada.saque.valor, cc.cobs[2].valor.retirada.saque.valor);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.retirada.saque.modalidadeAlteracao, cc.cobs[2].valor.retirada.saque.modalidadeAlteracao);
    CheckTrue(fACBrPixCobsConsultadas.cobs[2].valor.retirada.saque.modalidadeAgente = cc.cobs[2].valor.retirada.saque.modalidadeAgente);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].valor.retirada.saque.prestadorDoServicoDeSaque, cc.cobs[2].valor.retirada.saque.prestadorDoServicoDeSaque);
    CheckEquals(fACBrPixCobsConsultadas.cobs[2].chave, cc.cobs[2].chave);

    CheckEquals(fACBrPixCobsConsultadas.cobs[3].calendario.criacao, cc.cobs[3].calendario.criacao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].calendario.criacao_Bias, cc.cobs[3].calendario.criacao_Bias);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].calendario.expiracao, cc.cobs[3].calendario.expiracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].txId, cc.cobs[3].txId);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].revisao, cc.cobs[3].revisao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].loc.id, cc.cobs[3].loc.id);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].loc.location, cc.cobs[3].loc.location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[3].loc.tipoCob = cc.cobs[3].loc.tipoCob);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].location, cc.cobs[3].location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[3].status = cc.cobs[3].status);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].devedor.cnpj, cc.cobs[3].devedor.cnpj);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].devedor.nome, cc.cobs[3].devedor.nome);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.original, cc.cobs[3].valor.original);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.modalidadeAlteracao, cc.cobs[3].valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.retirada.saque.valor, cc.cobs[3].valor.retirada.saque.valor);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.retirada.saque.modalidadeAlteracao, cc.cobs[3].valor.retirada.saque.modalidadeAlteracao);
    CheckTrue(fACBrPixCobsConsultadas.cobs[3].valor.retirada.saque.modalidadeAgente = cc.cobs[3].valor.retirada.saque.modalidadeAgente);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].valor.retirada.saque.prestadorDoServicoDeSaque, cc.cobs[3].valor.retirada.saque.prestadorDoServicoDeSaque);
    CheckEquals(fACBrPixCobsConsultadas.cobs[3].chave, cc.cobs[3].chave);

    CheckEquals(fACBrPixCobsConsultadas.cobs[4].calendario.criacao, cc.cobs[4].calendario.criacao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].calendario.criacao_Bias, cc.cobs[4].calendario.criacao_Bias);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].calendario.expiracao, cc.cobs[4].calendario.expiracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].txId, cc.cobs[4].txId);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].revisao, cc.cobs[4].revisao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].loc.id, cc.cobs[4].loc.id);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].loc.location, cc.cobs[4].loc.location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[4].loc.tipoCob = cc.cobs[4].loc.tipoCob);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].location, cc.cobs[4].location);
    CheckTrue(fACBrPixCobsConsultadas.cobs[4].status = cc.cobs[4].status);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].devedor.cnpj, cc.cobs[4].devedor.cnpj);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].devedor.nome, cc.cobs[4].devedor.nome);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.original, cc.cobs[4].valor.original);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.modalidadeAlteracao, cc.cobs[4].valor.modalidadeAlteracao);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.retirada.troco.valor, cc.cobs[4].valor.retirada.troco.valor);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.retirada.troco.modalidadeAlteracao, cc.cobs[4].valor.retirada.troco.modalidadeAlteracao);
    CheckTrue(fACBrPixCobsConsultadas.cobs[4].valor.retirada.troco.modalidadeAgente = cc.cobs[4].valor.retirada.troco.modalidadeAgente);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].valor.retirada.troco.prestadorDoServicoDeSaque, cc.cobs[4].valor.retirada.troco.prestadorDoServicoDeSaque);
    CheckEquals(fACBrPixCobsConsultadas.cobs[4].chave, cc.cobs[4].chave);
  finally
    cc.Free;
  end;
end;

{ TTestCobRevisada }

procedure TTestCobRevisada.SetUp;
begin
  inherited SetUp;
  fACBrPixCobRevisada := TACBrPIXCobRevisada.Create('');
  fJSON := ACBrStr(
  '{'+
  	'"loc": {'+
  		'"id": 7768'+
  	'},'+
  	'"devedor": {'+
  		'"cpf": "12345678909",'+
  		'"nome": "Francisco da Silva"'+
  	'},'+
  	'"valor": {'+
  		'"original": "123.45"'+
  	'},'+
  	'"solicitacaoPagador": "Cobrança dos serviços prestados."'+
  '}' );
end;

procedure TTestCobRevisada.TearDown;
begin
  fACBrPixCobRevisada.Free;
  inherited TearDown;
end;

procedure TTestCobRevisada.AtribuirELerValoresCobBody3;
begin
  fACBrPixCobRevisada.AsJSON := fJSON;

  CheckEquals(fACBrPixCobRevisada.loc.id, 7768);
  CheckEquals(fACBrPixCobRevisada.devedor.cpf, '12345678909');
  CheckEquals(fACBrPixCobRevisada.devedor.nome, 'Francisco da Silva');
  CheckEquals(fACBrPixCobRevisada.valor.original, 123.45);
  CheckEquals(fACBrPixCobRevisada.solicitacaoPagador, ACBrStr('Cobrança dos serviços prestados.'));
end;

procedure TTestCobRevisada.AtribuirLerReatribuirECompararCobBody3;
var
  cr: TACBrPIXCobRevisada;
  s: String;
begin
  fACBrPixCobRevisada.AsJSON := fJSON;
  s := fACBrPixCobRevisada.AsJSON;
  cr := TACBrPIXCobRevisada.Create('');
  try
    cr.AsJSON := s;

    CheckEquals(fACBrPixCobRevisada.loc.id, cr.loc.id);
    CheckEquals(fACBrPixCobRevisada.devedor.cpf, cr.devedor.cpf);
    CheckEquals(fACBrPixCobRevisada.devedor.nome, cr.devedor.nome);
    CheckEquals(fACBrPixCobRevisada.valor.original, cr.valor.original);
    CheckEquals(fACBrPixCobRevisada.solicitacaoPagador, cr.solicitacaoPagador);
  finally
    cr.Free;
  end;
end;

{ TTestCobVSolicitada }

procedure TTestCobVSolicitada.SetUp;
begin
  inherited SetUp;
  fACBrPixCobVSolicitada := TACBrPIXCobVSolicitada.Create('');
  fJSON := ACBrStr(
  '{'+
    '"calendario": {'+
      '"dataDeVencimento": "2020-12-31",'+
      '"validadeAposVencimento": 30'+
    '},'+
    '"loc": {'+
      '"id": 789'+
    '},'+
    '"devedor": {'+
      '"logradouro": "Alameda Souza, Numero 80, Bairro Braz",'+
      '"cidade": "Recife",'+
      '"uf": "PE",'+
      '"cep": "70011750",'+
      '"cpf": "12345678909",'+
      '"nome": "Francisco da Silva"'+
    '},'+
    '"valor": {'+
      '"original": "123.45",'+
      '"multa": {'+
        '"modalidade": "2",'+
        '"valorPerc": "15.00"'+
      '},'+
      '"juros": {'+
        '"modalidade": "2",'+
        '"valorPerc": "2.00"'+
      '},'+
      '"desconto": {'+
        '"modalidade": "1",'+
        '"descontoDataFixa": ['+
          '{'+
            '"data": "2020-11-30",'+
            '"valorPerc": "30.00"'+
          '}'+
        ']'+
      '}'+
    '},'+
    '"chave": "5f84a4c5-c5cb-4599-9f13-7eb4d419dacc",'+
    '"solicitacaoPagador": "Cobrança dos serviços prestados."'+
  '}' );

end;

procedure TTestCobVSolicitada.TearDown;
begin
  fACBrPixCobVSolicitada.Free;
  inherited TearDown;
end;

procedure TTestCobVSolicitada.AtribuirELerValoresCobrancaComVencimento1;
begin
  fACBrPixCobVSolicitada.AsJSON := fJSON;

  CheckEquals(fACBrPixCobVSolicitada.calendario.dataDeVencimento, EncodeDate(2020,12,31));
  CheckEquals(fACBrPixCobVSolicitada.calendario.validadeAposVencimento, 30);
  CheckEquals(fACBrPixCobVSolicitada.loc.id, 789);
  CheckEquals(fACBrPixCobVSolicitada.devedor.logradouro, 'Alameda Souza, Numero 80, Bairro Braz');
  CheckEquals(fACBrPixCobVSolicitada.devedor.cidade, 'Recife');
  CheckEquals(fACBrPixCobVSolicitada.devedor.uf, 'PE');
  CheckEquals(fACBrPixCobVSolicitada.devedor.cep, '70011750');
  CheckEquals(fACBrPixCobVSolicitada.devedor.cpf, '12345678909');
  CheckEquals(fACBrPixCobVSolicitada.devedor.nome, 'Francisco da Silva');
  CheckEquals(fACBrPixCobVSolicitada.valor.original, 123.45);
  CheckEquals(Ord(fACBrPixCobVSolicitada.valor.multa.modalidade), 2);
  CheckEquals(fACBrPixCobVSolicitada.valor.multa.valorPerc, 15);
  CheckEquals(Ord(fACBrPixCobVSolicitada.valor.juros.modalidade), 2);
  CheckEquals(fACBrPixCobVSolicitada.valor.juros.valorPerc, 2);
  CheckEquals(Ord(fACBrPixCobVSolicitada.valor.desconto.modalidade), 1);
  CheckEquals(fACBrPixCobVSolicitada.valor.desconto.descontosDataFixa[0].data, EncodeDate(2020,11,30));
  CheckEquals(fACBrPixCobVSolicitada.valor.desconto.descontosDataFixa[0].valorPerc, 30);
  CheckEquals(fACBrPixCobVSolicitada.chave, '5f84a4c5-c5cb-4599-9f13-7eb4d419dacc');
  CheckEquals(fACBrPixCobVSolicitada.solicitacaoPagador, ACBrStr('Cobrança dos serviços prestados.'));
end;

procedure TTestCobVSolicitada.AtribuirLerReatribuirECompararCobrancaComVencimento1;
var
  cs: TACBrPIXCobVSolicitada;
  s: String;
begin
  fACBrPixCobVSolicitada.AsJSON := fJSON;
  s := fACBrPixCobVSolicitada.AsJSON;
  cs := TACBrPIXCobVSolicitada.Create('');
  try
    cs.AsJSON := s;

    CheckEquals(fACBrPixCobVSolicitada.calendario.dataDeVencimento, cs.calendario.dataDeVencimento);
    CheckEquals(fACBrPixCobVSolicitada.calendario.validadeAposVencimento, cs.calendario.validadeAposVencimento);
    CheckEquals(fACBrPixCobVSolicitada.loc.id, cs.loc.id);
    CheckEquals(fACBrPixCobVSolicitada.devedor.logradouro, cs.devedor.logradouro);
    CheckEquals(fACBrPixCobVSolicitada.devedor.cidade, cs.devedor.cidade);
    CheckEquals(fACBrPixCobVSolicitada.devedor.uf, cs.devedor.uf);
    CheckEquals(fACBrPixCobVSolicitada.devedor.cep, cs.devedor.cep);
    CheckEquals(fACBrPixCobVSolicitada.devedor.cpf, cs.devedor.cpf);
    CheckEquals(fACBrPixCobVSolicitada.devedor.nome, cs.devedor.nome);
    CheckEquals(fACBrPixCobVSolicitada.valor.original, cs.valor.original);
    CheckEquals(Ord(fACBrPixCobVSolicitada.valor.multa.modalidade), Ord(cs.valor.multa.modalidade));
    CheckEquals(fACBrPixCobVSolicitada.valor.multa.valorPerc, cs.valor.multa.valorPerc);
    CheckEquals(Ord(fACBrPixCobVSolicitada.valor.juros.modalidade), Ord(cs.valor.juros.modalidade));
    CheckEquals(fACBrPixCobVSolicitada.valor.juros.valorPerc, cs.valor.juros.valorPerc);
    CheckEquals(Ord(fACBrPixCobVSolicitada.valor.desconto.modalidade), Ord(cs.valor.desconto.modalidade));
    CheckEquals(fACBrPixCobVSolicitada.valor.desconto.descontosDataFixa[0].data, cs.valor.desconto.descontosDataFixa[0].data);
    CheckEquals(fACBrPixCobVSolicitada.valor.desconto.descontosDataFixa[0].valorPerc, cs.valor.desconto.descontosDataFixa[0].valorPerc);
    CheckEquals(fACBrPixCobVSolicitada.chave, cs.chave);
    CheckEquals(fACBrPixCobVSolicitada.solicitacaoPagador, cs.solicitacaoPagador);
  finally
    cs.Free;
  end;
end;


{ TTestCobVGerada }

procedure TTestCobVGerada.SetUp;
begin
  inherited SetUp;
  fJSON := ACBrStr(
  '{'+
    '"calendario": {'+
      '"criacao": "2020-09-09T20:15:00.358Z",'+
      '"dataDeVencimento": "2020-12-31",'+
      '"validadeAposVencimento": 30'+
    '},'+
    '"txid": "7978c0c97ea847e78e8849634473c1f1",'+
    '"revisao": 0,'+
    '"loc": {'+
      '"id": 789,'+
      '"location": "pix.example.com/qr/c2/cobv/9d36b84fc70b478fb95c12729b90ca25",'+
      '"tipoCob": "cobv"'+
    '},'+
    '"status": "ATIVA",'+
    '"devedor": {'+
      '"logradouro": "Alameda Souza, Numero 80, Bairro Braz",'+
      '"cidade": "Recife",'+
      '"uf": "PE",'+
      '"cep": "70011750",'+
      '"cpf": "12345678909",'+
      '"nome": "Francisco da Silva"'+
    '},'+
    '"recebedor": {'+
      '"logradouro": "Rua 15 Numero 1200, Bairro São Luiz",'+
      '"cidade": "São Paulo",'+
      '"uf": "SP",'+
      '"cep": "70800100",'+
      '"cnpj": "56989000019533",'+
      '"nome": "Empresa de Logística SA"'+
    '},'+
    '"valor": {'+
      '"original": "123.45"'+
    '},'+
    '"chave": "5f84a4c5-c5cb-4599-9f13-7eb4d419dacc",'+
    '"solicitacaoPagador": "Cobrança dos serviços prestados."'+
  '}' );
  fACBrPixCobVGerada := TACBrPIXCobVGerada.Create('');
end;

procedure TTestCobVGerada.TearDown;
begin
  fACBrPixCobVGerada.Free;
  inherited TearDown;
end;

procedure TTestCobVGerada.AtribuirELerValoresCobrancaComVencimento1;
begin
  fACBrPixCobVGerada.AsJSON := fJSON;

  CheckEquals(fACBrPixCobVGerada.calendario.criacao, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckEquals(fACBrPixCobVGerada.calendario.dataDeVencimento, EncodeDate(2020,12,31));
  CheckEquals(fACBrPixCobVGerada.calendario.validadeAposVencimento, 30);
  CheckEquals(fACBrPixCobVGerada.txId, '7978c0c97ea847e78e8849634473c1f1');
  CheckEquals(fACBrPixCobVGerada.revisao, 0);
  CheckEquals(fACBrPixCobVGerada.loc.id, 789);
  CheckEquals(fACBrPixCobVGerada.loc.location, 'pix.example.com/qr/c2/cobv/9d36b84fc70b478fb95c12729b90ca25');
  CheckTrue(fACBrPixCobVGerada.loc.tipoCob = tcoCobV);
  CheckTrue(fACBrPixCobVGerada.status = stcATIVA);
  CheckEquals(fACBrPixCobVGerada.devedor.logradouro, 'Alameda Souza, Numero 80, Bairro Braz');
  CheckEquals(fACBrPixCobVGerada.devedor.cidade, 'Recife');
  CheckEquals(fACBrPixCobVGerada.devedor.uf, 'PE');
  CheckEquals(fACBrPixCobVGerada.devedor.cep, '70011750');
  CheckEquals(fACBrPixCobVGerada.devedor.cpf, '12345678909');
  CheckEquals(fACBrPixCobVGerada.devedor.nome, 'Francisco da Silva');
  CheckEquals(fACBrPixCobVGerada.recebedor.logradouro, ACBrStr('Rua 15 Numero 1200, Bairro São Luiz'));
  CheckEquals(fACBrPixCobVGerada.recebedor.cidade, ACBrStr('São Paulo'));
  CheckEquals(fACBrPixCobVGerada.recebedor.uf, 'SP');
  CheckEquals(fACBrPixCobVGerada.recebedor.cep, '70800100');
  CheckEquals(fACBrPixCobVGerada.recebedor.cnpj, '56989000019533');
  CheckEquals(fACBrPixCobVGerada.recebedor.nome, ACBrStr('Empresa de Logística SA'));
  CheckEquals(fACBrPixCobVGerada.valor.original, 123.45);
  CheckEquals(fACBrPixCobVGerada.chave, '5f84a4c5-c5cb-4599-9f13-7eb4d419dacc');
  CheckEquals(fACBrPixCobVGerada.solicitacaoPagador, ACBrStr('Cobrança dos serviços prestados.'));
end;

procedure TTestCobVGerada.AtribuirLerReatribuirECompararCobrancaComVencimento1;
var
  cg: TACBrPIXCobVGerada;
  s: String;
begin
  fACBrPixCobVGerada.AsJSON := fJSON;
  s := fACBrPixCobVGerada.AsJSON;
  cg := TACBrPIXCobVGerada.Create('');
  try
    cg.AsJSON := s;

    CheckEquals(fACBrPixCobVGerada.calendario.criacao, cg.calendario.criacao);
    CheckEquals(fACBrPixCobVGerada.calendario.dataDeVencimento, cg.calendario.dataDeVencimento);
    CheckEquals(fACBrPixCobVGerada.calendario.validadeAposVencimento, cg.calendario.validadeAposVencimento);
    CheckEquals(fACBrPixCobVGerada.txId, cg.txId);
    CheckEquals(fACBrPixCobVGerada.revisao, cg.revisao);
    CheckEquals(fACBrPixCobVGerada.loc.id, cg.loc.id);
    CheckEquals(fACBrPixCobVGerada.loc.location, cg.loc.location);
    CheckTrue(fACBrPixCobVGerada.loc.tipoCob = cg.loc.tipoCob);
    CheckTrue(fACBrPixCobVGerada.status = cg.status);
    CheckEquals(fACBrPixCobVGerada.devedor.logradouro, cg.devedor.logradouro);
    CheckEquals(fACBrPixCobVGerada.devedor.cidade, cg.devedor.cidade);
    CheckEquals(fACBrPixCobVGerada.devedor.uf, cg.devedor.uf);
    CheckEquals(fACBrPixCobVGerada.devedor.cep, cg.devedor.cep);
    CheckEquals(fACBrPixCobVGerada.devedor.cpf, cg.devedor.cpf);
    CheckEquals(fACBrPixCobVGerada.devedor.nome, cg.devedor.nome);
    CheckEquals(fACBrPixCobVGerada.recebedor.logradouro, cg.recebedor.logradouro);
    CheckEquals(fACBrPixCobVGerada.recebedor.cidade, cg.recebedor.cidade);
    CheckEquals(fACBrPixCobVGerada.recebedor.uf, cg.recebedor.uf);
    CheckEquals(fACBrPixCobVGerada.recebedor.cep, cg.recebedor.cep);
    CheckEquals(fACBrPixCobVGerada.recebedor.cnpj, cg.recebedor.cnpj);
    CheckEquals(fACBrPixCobVGerada.recebedor.nome, cg.recebedor.nome);
    CheckEquals(fACBrPixCobVGerada.valor.original, cg.valor.original);
    CheckEquals(fACBrPixCobVGerada.chave, cg.chave);
    CheckEquals(fACBrPixCobVGerada.solicitacaoPagador, cg.solicitacaoPagador);
  finally
    cg.Free;
  end;
end;


{ TTestCobsVConsultadas }

procedure TTestCobsVConsultadas.SetUp;
begin
  inherited SetUp;
  fACBrPixCobsVConsultadas := TACBrPIXCobsVConsultadas.Create('');
  fJSON := ACBrStr(
    '{'+
  	  '"parametros": {'+
  		  '"inicio": "2020-04-01T00:00:00Z",'+
  		  '"fim": "2020-04-01T23:59:59Z",'+
  		  '"paginacao": {'+
  			  '"paginaAtual": 0,'+
  			  '"itensPorPagina": 100,'+
  			  '"quantidadeDePaginas": 1,'+
  			  '"quantidadeTotalDeItens": 1'+
  		  '}'+
  	  '},'+
  	  '"cobs": ['+
  		  '{'+
  			  '"calendario": {'+
  				  '"criacao": "2020-09-09T20:15:00.358Z",'+
  				  '"dataDeVencimento": "2020-12-31",'+
  				  '"validadeAposVencimento": 30'+
  			  '},'+
  			  '"txid": "7978c0c97ea847e78e8849634473c1f1",'+
  			  '"revisao": 0,'+
  			  '"loc": {'+
  				  '"id": 789,'+
  				  '"location": "pix.example.com/qr/c2/cobv/9d36b84fc70b478fb95c12729b90ca25",'+
  				  '"tipoCob": "cobv"'+
  			  '},'+
  			  '"status": "ATIVA",'+
  			  '"devedor": {'+
  				  '"logradouro": "Alameda Souza, Numero 80, Bairro Braz",'+
  				  '"cidade": "Recife",'+
  				  '"uf": "PE",'+
  				  '"cep": "70011750",'+
  				  '"cpf": "12345678909",'+
  				  '"nome": "Francisco da Silva"'+
  			  '},'+
  			  '"recebedor": {'+
  				  '"logradouro": "Rua 15 Numero 1200, Bairro São Luiz",'+
  				  '"cidade": "São Paulo",'+
  				  '"uf": "SP",'+
  				  '"cep": "70800100",'+
  				  '"cnpj": "56989000019533",'+
  				  '"nome": "Empresa de Logística SA"'+
  			  '},'+
  			  '"valor": {'+
  				  '"original": "123.45"'+
  			  '},'+
  			  '"chave": "5f84a4c5-c5cb-4599-9f13-7eb4d419dacc",'+
  			  '"solicitacaoPagador": "Cobrança dos serviços prestados."'+
  		  '}'+
  	  ']'+
    '}' );
end;

procedure TTestCobsVConsultadas.TearDown;
begin
  inherited TearDown;
end;

procedure TTestCobsVConsultadas.AtribuirELerValores;
begin
  fACBrPixCobsVConsultadas.AsJSON := fJSON;

  CheckEquals(fACBrPixCobsVConsultadas.parametros.inicio, EncodeDateTime(2020,04,01,00,00,00,0));
  CheckEquals(fACBrPixCobsVConsultadas.parametros.fim, EncodeDateTime(2020,04,01,23,59,59,0));
  CheckEquals(fACBrPixCobsVConsultadas.parametros.paginacao.paginaAtual, 0);
  CheckEquals(fACBrPixCobsVConsultadas.parametros.paginacao.itensPorPagina, 100);
  CheckEquals(fACBrPixCobsVConsultadas.parametros.paginacao.quantidadeDePaginas, 1);
  CheckEquals(fACBrPixCobsVConsultadas.parametros.paginacao.quantidadeTotalDeItens, 1);
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].calendario.criacao, EncodeDateTime(2020,09,09,20,15,00,358));
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].calendario.dataDeVencimento, EncodeDate(2020,12,31));
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].calendario.validadeAposVencimento, 30);
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].txId, '7978c0c97ea847e78e8849634473c1f1');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].revisao, 0);
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].loc.id, 789);
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].loc.location, 'pix.example.com/qr/c2/cobv/9d36b84fc70b478fb95c12729b90ca25');
  CheckTrue(fACBrPixCobsVConsultadas.cobs[0].loc.tipoCob = tcoCobV);
  CheckTrue(fACBrPixCobsVConsultadas.cobs[0].status = stcATIVA);
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.logradouro, 'Alameda Souza, Numero 80, Bairro Braz');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.cidade, 'Recife');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.uf, 'PE');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.cep, '70011750');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.cpf, '12345678909');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.nome, 'Francisco da Silva');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.logradouro, ACBrStr('Rua 15 Numero 1200, Bairro São Luiz'));
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.cidade, ACBrStr('São Paulo'));
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.uf, 'SP');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.cep, '70800100');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.cnpj, '56989000019533');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.nome, ACBrStr('Empresa de Logística SA'));
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].valor.original, 123.45);
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].chave, '5f84a4c5-c5cb-4599-9f13-7eb4d419dacc');
  CheckEquals(fACBrPixCobsVConsultadas.cobs[0].solicitacaoPagador, ACBrStr('Cobrança dos serviços prestados.'));
end;

procedure TTestCobsVConsultadas.AtribuirLerReatribuirEComparar;
var
  cg: TACBrPIXCobsVConsultadas;
  s: String;
begin
  fACBrPixCobsVConsultadas.AsJSON := fJSON;
  s := fACBrPixCobsVConsultadas.AsJSON;
  cg := TACBrPIXCobsVConsultadas.Create('');
  try
    cg.AsJSON := s;

    CheckEquals(fACBrPixCobsVConsultadas.parametros.inicio, cg.parametros.inicio);
    CheckEquals(fACBrPixCobsVConsultadas.parametros.fim, cg.parametros.fim);
    CheckEquals(fACBrPixCobsVConsultadas.parametros.paginacao.paginaAtual, cg.parametros.paginacao.paginaAtual);
    CheckEquals(fACBrPixCobsVConsultadas.parametros.paginacao.itensPorPagina, cg.parametros.paginacao.itensPorPagina);
    CheckEquals(fACBrPixCobsVConsultadas.parametros.paginacao.quantidadeDePaginas, cg.parametros.paginacao.quantidadeDePaginas);
    CheckEquals(fACBrPixCobsVConsultadas.parametros.paginacao.quantidadeTotalDeItens, cg.parametros.paginacao.quantidadeTotalDeItens);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].calendario.criacao, cg.cobs[0].calendario.criacao);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].calendario.dataDeVencimento, cg.cobs[0].calendario.dataDeVencimento);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].calendario.validadeAposVencimento, cg.cobs[0].calendario.validadeAposVencimento);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].txId, cg.cobs[0].txId);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].revisao, cg.cobs[0].revisao);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].loc.id, cg.cobs[0].loc.id);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].loc.location, cg.cobs[0].loc.location);
    CheckTrue(fACBrPixCobsVConsultadas.cobs[0].loc.tipoCob = cg.cobs[0].loc.tipoCob);
    CheckTrue(fACBrPixCobsVConsultadas.cobs[0].status = cg.cobs[0].status);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.logradouro, cg.cobs[0].devedor.logradouro);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.cidade, cg.cobs[0].devedor.cidade);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.uf, cg.cobs[0].devedor.uf);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.cep, cg.cobs[0].devedor.cep);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.cpf, cg.cobs[0].devedor.cpf);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].devedor.nome, cg.cobs[0].devedor.nome);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.logradouro, cg.cobs[0].recebedor.logradouro);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.cidade, cg.cobs[0].recebedor.cidade);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.uf, cg.cobs[0].recebedor.uf);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.cep, cg.cobs[0].recebedor.cep);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.cnpj, cg.cobs[0].recebedor.cnpj);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].recebedor.nome, cg.cobs[0].recebedor.nome);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].valor.original, cg.cobs[0].valor.original);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].chave, cg.cobs[0].chave);
    CheckEquals(fACBrPixCobsVConsultadas.cobs[0].solicitacaoPagador, cg.cobs[0].solicitacaoPagador);
  finally
    cg.Free;
  end;
end;

{ TTestCobVRevisada }

procedure TTestCobVRevisada.SetUp;
begin
  inherited SetUp;
  fACBrPixCobVRevisada := TACBrPIXCobVRevisada.Create('');
  fJSON1 := ACBrStr(
  '{'+
    '"loc": {'+
      '"id": 789'+
    '},'+
    '"devedor": {'+
      '"logradouro": "Alameda Souza, Numero 80, Bairro Braz",'+
      '"cidade": "Recife",'+
      '"uf": "PE",'+
      '"cep": "70011750",'+
      '"cpf": "12345678909",'+
      '"nome": "Francisco da Silva"'+
    '},'+
    '"valor": {'+
      '"original": "123.45"'+
    '},'+
    '"solicitacaoPagador": "Cobrança dos serviços prestados."'+
  '}');

  fJSON2 := ACBrStr(
  '{'+
    '"valor": {'+
      '"original": "567.89"'+
    '},'+
    '"solicitacaoPagador": "Informar cartão fidelidade"'+
  '}' );

  fJSON3 :=
  '{'+
    '"status": "REMOVIDA_PELO_USUARIO_RECEBEDOR"'+
  '}';
end;

procedure TTestCobVRevisada.TearDown;
begin
  fACBrPixCobVRevisada.Free;
  inherited TearDown;
end;

procedure TTestCobVRevisada.AtribuirELerValoresRevisaoCobrancaComVencimento1;
begin
  fACBrPixCobVRevisada.AsJSON := fJSON1;

  CheckEquals(fACBrPixCobVRevisada.loc.id, 789);
  CheckEquals(fACBrPixCobVRevisada.devedor.logradouro, 'Alameda Souza, Numero 80, Bairro Braz');
  CheckEquals(fACBrPixCobVRevisada.devedor.cidade, 'Recife');
  CheckEquals(fACBrPixCobVRevisada.devedor.uf, 'PE');
  CheckEquals(fACBrPixCobVRevisada.devedor.cep, '70011750');
  CheckEquals(fACBrPixCobVRevisada.devedor.cpf, '12345678909');
  CheckEquals(fACBrPixCobVRevisada.devedor.nome, 'Francisco da Silva');
  CheckEquals(fACBrPixCobVRevisada.valor.original,123.45);
  CheckEquals(fACBrPixCobVRevisada.solicitacaoPagador, ACBrStr('Cobrança dos serviços prestados.'));
end;

procedure TTestCobVRevisada.AtribuirLerReatribuirECompararRevisaoCobrancaComVencimento1;
var
  cr: TACBrPIXCobVRevisada;
  s: String;
begin
  fACBrPixCobVRevisada.AsJSON := fJSON1;
  s := fACBrPixCobVRevisada.AsJSON;
  cr := TACBrPIXCobVRevisada.Create('');
  try
    cr.AsJSON := s;

    CheckEquals(fACBrPixCobVRevisada.loc.id, cr.loc.id);
    CheckEquals(fACBrPixCobVRevisada.devedor.logradouro, cr.devedor.logradouro);
    CheckEquals(fACBrPixCobVRevisada.devedor.cidade, cr.devedor.cidade);
    CheckEquals(fACBrPixCobVRevisada.devedor.uf, cr.devedor.uf);
    CheckEquals(fACBrPixCobVRevisada.devedor.cep, cr.devedor.cep);
    CheckEquals(fACBrPixCobVRevisada.devedor.cpf, cr.devedor.cpf);
    CheckEquals(fACBrPixCobVRevisada.devedor.nome, cr.devedor.nome);
    CheckEquals(fACBrPixCobVRevisada.valor.original, cr.valor.original);
    CheckEquals(fACBrPixCobVRevisada.solicitacaoPagador, cr.solicitacaoPagador);
  finally
    cr.Free;
  end;
end;

procedure TTestCobVRevisada.AtribuirELerValoresRevisaoCobranca2;
begin
  fACBrPixCobVRevisada.AsJSON := fJSON2;

  CheckEquals(fACBrPixCobVRevisada.valor.original, 567.89);
  CheckEquals(fACBrPixCobVRevisada.solicitacaoPagador, ACBrStr('Informar cartão fidelidade'));
end;

procedure TTestCobVRevisada.AtribuirLerReatribuirECompararRevisaoCobranca2;
var
  cr: TACBrPIXCobVRevisada;
  s: String;
begin
  fACBrPixCobVRevisada.AsJSON := fJSON2;
  s := fACBrPixCobVRevisada.AsJSON;
  cr := TACBrPIXCobVRevisada.Create('');
  try
    cr.AsJSON := s;
    CheckEquals(fACBrPixCobVRevisada.valor.original, cr.valor.original);
    CheckEquals(fACBrPixCobVRevisada.solicitacaoPagador, cr.solicitacaoPagador);
  finally
    cr.Free;
  end;
end;

procedure TTestCobVRevisada.AtribuirELerValoresRevisaoCobranca3;
begin
  fACBrPixCobVRevisada.AsJSON := fJSON3;

  CheckTrue(fACBrPixCobVRevisada.status = stcREMOVIDA_PELO_USUARIO_RECEBEDOR);
end;

procedure TTestCobVRevisada.AtribuirLerReatribuirECompararRevisaoCobranca3;
var
  cr: TACBrPIXCobVRevisada;
  s: String;
begin
  fACBrPixCobVRevisada.AsJSON := fJSON3;
  s := fACBrPixCobVRevisada.AsJSON;
  cr := TACBrPIXCobVRevisada.Create('');
  try
    cr.AsJSON := s;
    CheckTrue(fACBrPixCobVRevisada.status = cr.status);
  finally
    cr.Free;
  end;
end;

{ TTestLoteCobVBody }

procedure TTestLoteCobVBody.SetUp;
begin
  inherited SetUp;
  fACBrPixLoteCobVBody := TACBrPIXLoteCobVBody.Create('');
  fJSON := ACBrStr(
  '{'+
    '"descricao": "Cobranças dos alunos do turno vespertino",'+
    '"cobsv": ['+
      '{'+
        '"calendario": {'+
          '"dataDeVencimento": "2020-12-31",'+
          '"validadeAposVencimento": 30'+
        '},'+
        '"txid": "fb2761260e554ad593c7226beb5cb650",'+
        '"loc": {'+
          '"id": 789'+
        '},'+
        '"devedor": {'+
          '"logradouro": "Alameda Souza, Numero 80, Bairro Braz",'+
          '"cidade": "Recife",'+
          '"uf": "PE",'+
          '"cep": "70011750",'+
          '"cpf": "08577095428",'+
          '"nome": "João Souza"'+
        '},'+
        '"valor": {'+
          '"original": "100.00"'+
        '},'+
        '"chave": "7c084cd4-54af-4172-a516-a7d1a12b75cc",'+
        '"solicitacaoPagador": "Informar matrícula"'+
      '},'+
      '{'+
        '"calendario": {'+
          '"dataDeVencimento": "2020-12-31",'+
          '"validadeAposVencimento": 30'+
        '},'+
        '"txid": "7978c0c97ea847e78e8849634473c1f1",'+
        '"loc": {'+
          '"id": 57221'+
        '},'+
        '"devedor": {'+
          '"logradouro": "Rua 15, Numero 1, Bairro Campo Grande",'+
          '"cidade": "Recife",'+
          '"uf": "PE",'+
          '"cep": "70055751",'+
          '"cpf": "15311295449",'+
          '"nome": "Manoel Silva"'+
        '},'+
        '"valor": {'+
          '"original": "100.00"'+
        '},'+
        '"chave": "7c084cd4-54af-4172-a516-a7d1a12b75cc",'+
        '"solicitacaoPagador": "Informar matrícula"'+
      '}'+
    ']'+
  '}' );
end;

procedure TTestLoteCobVBody.TearDown;
begin
  fACBrPixLoteCobVBody.Free;
  inherited TearDown;
end;

procedure TTestLoteCobVBody.AtribuirELerValores;
begin
  fACBrPixLoteCobVBody.AsJSON := fJSON;

  CheckEquals(fACBrPixLoteCobVBody.descricao, ACBrStr('Cobranças dos alunos do turno vespertino'));
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].calendario.dataDeVencimento, EncodeDate(2020,12,31));
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].calendario.validadeAposVencimento, 30);
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].txId, 'fb2761260e554ad593c7226beb5cb650');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].loc.id, 789);
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.logradouro, 'Alameda Souza, Numero 80, Bairro Braz');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.cidade, 'Recife');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.uf, 'PE');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.cep, '70011750');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.cpf, '08577095428');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.nome, ACBrStr('João Souza'));
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].valor.original, 100);
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].chave, '7c084cd4-54af-4172-a516-a7d1a12b75cc');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[0].solicitacaoPagador, ACBrStr('Informar matrícula'));
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].calendario.dataDeVencimento, EncodeDate(2020,12,31));
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].calendario.validadeAposVencimento, 30);
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].txId, '7978c0c97ea847e78e8849634473c1f1');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].loc.id, 57221);
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.logradouro, 'Rua 15, Numero 1, Bairro Campo Grande');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.cidade, 'Recife');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.uf, 'PE');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.cep, '70055751');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.cpf, '15311295449');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.nome, 'Manoel Silva');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].valor.original, 100);
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].chave, '7c084cd4-54af-4172-a516-a7d1a12b75cc');
  CheckEquals(fACBrPixLoteCobVBody.cobsv[1].solicitacaoPagador, ACBrStr('Informar matrícula'));
end;

procedure TTestLoteCobVBody.AtribuirLerReatribuirEComparar;
var
  lc: TACBrPIXLoteCobVBody;
  s: String;
begin
  fACBrPixLoteCobVBody.AsJSON := fJSON;
  s := fACBrPixLoteCobVBody.AsJSON;
  lc := TACBrPIXLoteCobVBody.Create('');
  try
    lc.AsJSON := s;

    CheckEquals(fACBrPixLoteCobVBody.descricao, lc.descricao);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].calendario.dataDeVencimento, lc.cobsv[0].calendario.dataDeVencimento);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].calendario.validadeAposVencimento, lc.cobsv[0].calendario.validadeAposVencimento);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].txId, lc.cobsv[0].txId);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].loc.id, lc.cobsv[0].loc.id);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.logradouro, lc.cobsv[0].devedor.logradouro);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.cidade, lc.cobsv[0].devedor.cidade);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.uf, lc.cobsv[0].devedor.uf);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.cep, lc.cobsv[0].devedor.cep);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.cpf, lc.cobsv[0].devedor.cpf);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].devedor.nome, lc.cobsv[0].devedor.nome);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].valor.original, lc.cobsv[0].valor.original);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].chave, lc.cobsv[0].chave);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[0].solicitacaoPagador, lc.cobsv[0].solicitacaoPagador);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].calendario.dataDeVencimento, lc.cobsv[1].calendario.dataDeVencimento);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].calendario.validadeAposVencimento, lc.cobsv[1].calendario.validadeAposVencimento);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].txId, lc.cobsv[1].txId);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].loc.id, lc.cobsv[1].loc.id);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.logradouro, lc.cobsv[1].devedor.logradouro);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.cidade, lc.cobsv[1].devedor.cidade);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.uf, lc.cobsv[1].devedor.uf);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.cep, lc.cobsv[1].devedor.cep);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.cpf, lc.cobsv[1].devedor.cpf);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].devedor.nome, lc.cobsv[1].devedor.nome);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].valor.original, lc.cobsv[1].valor.original);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].chave, lc.cobsv[1].chave);
    CheckEquals(fACBrPixLoteCobVBody.cobsv[1].solicitacaoPagador, lc.cobsv[1].solicitacaoPagador);
  finally
    lc.Free;
  end;
end;

{ TTestLoteCobVBodyRevisado }

procedure TTestLoteCobVBodyRevisado.SetUp;
begin
  inherited SetUp;
  fACBrPixLoteCobVBodyRevisado := TACBrPIXLoteCobVBodyRevisado.Create('');
  fJSON := ACBrStr(
  '{'+
    '"cobsv": ['+
      '{'+
        '"calendario": {'+
          '"dataDeVencimento": "2020-01-10"'+
        '},'+
        '"txid": "fb2761260e554ad593c7226beb5cb650",'+
        '"valor": {'+
          '"original": "110.00"'+
        '}'+
      '},'+
      '{'+
        '"calendario": {'+
          '"dataDeVencimento": "2020-01-10"'+
        '},'+
        '"txid": "7978c0c97ea847e78e8849634473c1f1",'+
        '"valor": {'+
          '"original": "110.00"'+
        '}'+
      '}'+
    ']'+
  '}' );
end;

procedure TTestLoteCobVBodyRevisado.TearDown;
begin
  fACBrPixLoteCobVBodyRevisado.Free;
  inherited TearDown;
end;

procedure TTestLoteCobVBodyRevisado.AtribuirELerValores;
begin
  fACBrPixLoteCobVBodyRevisado.AsJSON := fJSON;

  CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[0].calendario.dataDeVencimento, EncodeDate(2020,01,10));
  CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[0].txId, 'fb2761260e554ad593c7226beb5cb650');
  CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[0].valor.original, 110);
  CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[1].calendario.dataDeVencimento, EncodeDate(2020,01,10));
  CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[1].txId, '7978c0c97ea847e78e8849634473c1f1');
  CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[1].valor.original, 110);
end;

procedure TTestLoteCobVBodyRevisado.AtribuirLerReatribuirEComparar;
var
  lc: TACBrPIXLoteCobVBodyRevisado;
  s: String;
begin
  fACBrPixLoteCobVBodyRevisado.AsJSON := fJSON;
  s := fACBrPixLoteCobVBodyRevisado.AsJSON;
  lc := TACBrPIXLoteCobVBodyRevisado.Create('');
  try
    lc.AsJSON := s;

    CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[0].calendario.dataDeVencimento, lc.cobsv[0].calendario.dataDeVencimento);
    CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[0].txId, lc.cobsv[0].txId);
    CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[0].valor.original, lc.cobsv[0].valor.original);
    CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[1].calendario.dataDeVencimento, lc.cobsv[1].calendario.dataDeVencimento);
    CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[1].txId, lc.cobsv[1].txId);
    CheckEquals(fACBrPixLoteCobVBodyRevisado.cobsv[1].valor.original, lc.cobsv[1].valor.original);
  finally
    lc.Free;
  end;
end;

{ TTestLoteCobVConsultado }

procedure TTestLoteCobVConsultado.SetUp;
begin
  inherited SetUp;
  fACBrPixLoteCobVConsultado := TACBrPIXLoteCobVConsultado.Create('');
  fJSON := ACBrStr(
  '{'+
    '"descricao": "Cobranças dos alunos do turno vespertino",'+
    '"criacao": "2020-11-01T20:15:00.358Z",'+
    '"cobsv": ['+
      '{'+
        '"criacao": "2020-11-01T20:15:00.358Z",'+
        '"txid": "fb2761260e554ad593c7226beb5cb650",'+
        '"status": "CRIADA"'+
      '},'+
      '{'+
        '"txid": "7978c0c97ea847e78e8849634473c1f1",'+
        '"status": "NEGADA",'+
        '"problema": {'+
          '"type": "https://pix.bcb.gov.br/api/v2/error/CobVOperacaoInvalida",'+
          '"title": "Cobrança inválida.",'+
          '"status": 400,'+
          '"detail": "A requisição que busca alterar ou criar uma cobrança com vencimento não respeita o _schema_ ou está semanticamente errada.",'+
          '"violacoes": ['+
            '{'+
              '"razao": "O objeto cobv.devedor não respeita o _schema_.",'+
              '"propriedade": "cobv.devedor"'+
            '}'+
          ']'+
        '}'+
      '}'+
    ']'+
  '}' );
end;

procedure TTestLoteCobVConsultado.TearDown;
begin
  fACBrPixLoteCobVConsultado.Free;
  inherited TearDown;
end;

procedure TTestLoteCobVConsultado.AtribuirELerValores;
begin
  fACBrPixLoteCobVConsultado.AsJSON := fJSON;

  CheckEquals(fACBrPixLoteCobVConsultado.descricao, ACBrStr('Cobranças dos alunos do turno vespertino'));
  CheckEquals(fACBrPixLoteCobVConsultado.criacao, EncodeDateTime(2020,11,01,20,15,00,358));
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[0].criacao, EncodeDateTime(2020,11,01,20,15,00,358));
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[0].txId, 'fb2761260e554ad593c7226beb5cb650');
  CheckTrue(fACBrPixLoteCobVConsultado.cobsv[0].status = stlCRIADA);
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].txId, '7978c0c97ea847e78e8849634473c1f1');
  CheckTrue(fACBrPixLoteCobVConsultado.cobsv[1].status = stlNEGADA);
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.type_uri, 'https://pix.bcb.gov.br/api/v2/error/CobVOperacaoInvalida');
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.title, ACBrStr('Cobrança inválida.'));
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.status, 400);
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.detail, ACBrStr('A requisição que busca alterar ou criar uma cobrança com vencimento não respeita o _schema_ ou está semanticamente errada.'));
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.violacoes[0].razao, ACBrStr('O objeto cobv.devedor não respeita o _schema_.'));
  CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.violacoes[0].propriedade, 'cobv.devedor');
end;

procedure TTestLoteCobVConsultado.AtribuirLerReatribuirEComparar;
var
  lc: TACBrPIXLoteCobVConsultado;
  s: String;
begin
  fACBrPixLoteCobVConsultado.AsJSON := fJSON;
  s := fACBrPixLoteCobVConsultado.AsJSON;
  lc := TACBrPIXLoteCobVConsultado.Create('');
  try
    lc.AsJSON := s;

    CheckEquals(fACBrPixLoteCobVConsultado.descricao, lc.descricao);
    CheckEquals(fACBrPixLoteCobVConsultado.criacao, lc.criacao);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[0].criacao, lc.cobsv[0].criacao);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[0].txId, lc.cobsv[0].txId);
    CheckTrue(fACBrPixLoteCobVConsultado.cobsv[0].status = lc.cobsv[0].status);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].txId, lc.cobsv[1].txId);
    CheckTrue(fACBrPixLoteCobVConsultado.cobsv[1].status = lc.cobsv[1].status);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.type_uri, lc.cobsv[1].problema.type_uri);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.title, lc.cobsv[1].problema.title);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.status, lc.cobsv[1].problema.status);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.detail, lc.cobsv[1].problema.detail);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.violacoes[0].razao, lc.cobsv[1].problema.violacoes[0].razao);
    CheckEquals(fACBrPixLoteCobVConsultado.cobsv[1].problema.violacoes[0].propriedade, lc.cobsv[1].problema.violacoes[0].propriedade);
  finally
    lc.Free;
  end;
end;

{ TTestLotesCobVConsultados }

procedure TTestLotesCobVConsultados.SetUp;
begin
  inherited SetUp;
  fACBrPIXLotesCobVConsultados := TACBrPIXLotesCobVConsultados.Create('');
  fJSON := ACBrStr(
  '{'+
    '"parametros": {'+
      '"inicio": "2020-01-01T00:00:00Z",'+
      '"fim": "2020-12-01T23:59:59Z",'+
      '"paginacao": {'+
        '"paginaAtual": 0,'+
  	'"itensPorPagina": 100,'+
  	'"quantidadeDePaginas": 1,'+
  	'"quantidadeTotalDeItens": 2'+
      '}'+
    '},'+
    '"lotes": ['+
      '{'+
        '"descricao": "Cobranças dos alunos do turno vespertino",'+
  	'"criacao": "2020-11-01T20:15:00.358Z",'+
  	'"cobsv": ['+
  	  '{'+
  	    '"criacao": "2020-11-01T20:15:00.358Z",'+
  	    '"txid": "fb2761260e554ad593c7226beb5cb650",'+
  	    '"status": "CRIADA"'+
  	  '},'+
  	  '{'+
  	    '"txid": "7978c0c97ea847e78e8849634473c1f1",'+
  	    '"status": "NEGADA",'+
  	    '"problema": {'+
  	      '"type": "https://pix.bcb.gov.br/api/v2/error/CobVOperacaoInvalida",'+
  	      '"title": "Cobrança inválida.",'+
  	      '"status": 400,'+
  	      '"detail": "A requisição que busca alterar ou criar uma cobrança com vencimento não respeita o _schema_ ou está semanticamente errada.",'+
  	      '"violacoes": ['+
  	        '{'+
  		  '"razao": "O objeto cobv.devedor não respeita o _schema_.",'+
  		  '"propriedade": "cobv.devedor"'+
  		'}'+
  	      ']'+
  	    '}'+
  	  '}'+
  	']'+
  	'},'+
  	'{'+
  	  '"descricao": "Cobranças dos assinantes anuais",'+
  	  '"criacao": "2020-11-17T20:00:00.358Z",'+
  	  '"cobsv": ['+
  	    '{'+
  	      '"criacao": "2020-11-17T20:00:00.358Z",'+
  	      '"txid": "06601eaa3822423fbe897f613b983e01",'+
  	      '"status": "CRIADA"'+
  	    '},'+
  	    '{'+
  	      '"criacao": "2020-11-17T20:00:00.358Z",'+
  	      '"txid": "4e07059760d54cf493de6e7f1fbfad9a",'+
  	      '"status": "CRIADA"'+
  	    '}'+
  	  ']'+
  	'}'+
      ']'+
  '}' );
end;

procedure TTestLotesCobVConsultados.TearDown;
begin
  fACBrPIXLotesCobVConsultados.Free;
  inherited TearDown;
end;

procedure TTestLotesCobVConsultados.AtribuirELerValores;
begin
  fACBrPIXLotesCobVConsultados.AsJSON := fJSON;

  CheckEquals(fACBrPIXLotesCobVConsultados.parametros.inicio, EncodeDateTime(2020,01,01,0,0,0,0));
  CheckEquals(fACBrPIXLotesCobVConsultados.parametros.fim, EncodeDateTime(2020,12,01,23,59,59,0));
  CheckEquals(fACBrPIXLotesCobVConsultados.parametros.paginacao.paginaAtual, 0);
  CheckEquals(fACBrPIXLotesCobVConsultados.parametros.paginacao.itensPorPagina, 100);
  CheckEquals(fACBrPIXLotesCobVConsultados.parametros.paginacao.quantidadeDePaginas, 1);
  CheckEquals(fACBrPIXLotesCobVConsultados.parametros.paginacao.quantidadeTotalDeItens, 2);
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].criacao, EncodeDateTime(2020,11,01,20,15,00,358));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].descricao, ACBrStr('Cobranças dos alunos do turno vespertino'));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[0].criacao, EncodeDateTime(2020,11,01,20,15,00,358));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[0].txId, 'fb2761260e554ad593c7226beb5cb650');
  CheckTrue(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[0].status = stlCRIADA);
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].txId, '7978c0c97ea847e78e8849634473c1f1');
  CheckTrue(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].status = stlNEGADA);
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.type_uri, 'https://pix.bcb.gov.br/api/v2/error/CobVOperacaoInvalida');
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.title, ACBrStr('Cobrança inválida.'));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.detail, ACBrStr('A requisição que busca alterar ou criar uma cobrança com vencimento não respeita o _schema_ ou está semanticamente errada.'));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.violacoes[0].razao, ACBrStr('O objeto cobv.devedor não respeita o _schema_.'));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.violacoes[0].propriedade, 'cobv.devedor');
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].criacao, EncodeDateTime(2020,11,17,20,00,00,358));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].descricao, ACBrStr('Cobranças dos assinantes anuais'));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[0].criacao, EncodeDateTime(2020,11,17,20,00,00,358));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[0].txId, '06601eaa3822423fbe897f613b983e01');
  CheckTrue(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[0].status = stlCRIADA);
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[1].criacao, EncodeDateTime(2020,11,17,20,00,00,358));
  CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[1].txId, '4e07059760d54cf493de6e7f1fbfad9a');
  CheckTrue(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[1].status = stlCRIADA);
end;

procedure TTestLotesCobVConsultados.AtribuirLerReatribuirEComparar;
var
  lc: TACBrPIXLotesCobVConsultados;
  s: String;
begin
  fACBrPIXLotesCobVConsultados.AsJSON := fJSON;
  s := fACBrPIXLotesCobVConsultados.AsJSON;
  lc := TACBrPIXLotesCobVConsultados.Create('');
  try
    lc.AsJSON := s;

    CheckEquals(fACBrPIXLotesCobVConsultados.parametros.inicio, lc.parametros.inicio);
    CheckEquals(fACBrPIXLotesCobVConsultados.parametros.fim, lc.parametros.fim);
    CheckEquals(fACBrPIXLotesCobVConsultados.parametros.paginacao.paginaAtual, lc.parametros.paginacao.paginaAtual);
    CheckEquals(fACBrPIXLotesCobVConsultados.parametros.paginacao.itensPorPagina, lc.parametros.paginacao.itensPorPagina);
    CheckEquals(fACBrPIXLotesCobVConsultados.parametros.paginacao.quantidadeDePaginas, lc.parametros.paginacao.quantidadeDePaginas);
    CheckEquals(fACBrPIXLotesCobVConsultados.parametros.paginacao.quantidadeTotalDeItens, lc.parametros.paginacao.quantidadeTotalDeItens);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].criacao, lc.lotes[0].criacao);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].descricao, lc.lotes[0].descricao);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[0].criacao, lc.lotes[0].cobsv[0].criacao);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[0].txId, lc.lotes[0].cobsv[0].txId);
    CheckTrue(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[0].status = lc.lotes[0].cobsv[0].status);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].txId, lc.lotes[0].cobsv[1].txId);
    CheckTrue(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].status = lc.lotes[0].cobsv[1].status);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.type_uri, lc.lotes[0].cobsv[1].problema.type_uri);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.title, lc.lotes[0].cobsv[1].problema.title);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.detail, lc.lotes[0].cobsv[1].problema.detail);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.violacoes[0].razao, lc.lotes[0].cobsv[1].problema.violacoes[0].razao);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[0].cobsv[1].problema.violacoes[0].propriedade, lc.lotes[0].cobsv[1].problema.violacoes[0].propriedade);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].criacao, lc.lotes[1].criacao);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].descricao, lc.lotes[1].descricao);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[0].criacao, lc.lotes[1].cobsv[0].criacao);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[0].txId, lc.lotes[1].cobsv[0].txId);
    CheckTrue(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[0].status = lc.lotes[1].cobsv[0].status);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[1].criacao, lc.lotes[1].cobsv[1].criacao);
    CheckEquals(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[1].txId, lc.lotes[1].cobsv[1].txId);
    CheckTrue(fACBrPIXLotesCobVConsultados.lotes[1].cobsv[1].status = lc.lotes[1].cobsv[1].status);
  finally
    lc.Free;
  end;
end;


initialization

  _RegisterTest('ACBrPIXCD.QRCode', TTestQRCodeEstatico);
  _RegisterTest('ACBrPIXCD.QRCode', TTestQRCodeDinamico);
  _RegisterTest('ACBrPIXCD.Schemas.Cob', TTestCobrancaImediataExemplo1);
  _RegisterTest('ACBrPIXCD.Schemas.Pix', TTestCobrancaImediataComSaquePIX);
  _RegisterTest('ACBrPIXCD.Schemas.Pix', TTestCobrancaImediataComSaquePIX1);
  _RegisterTest('ACBrPIXCD.Schemas.Pix', TTestCobrancaImediataComSaquePIX2);
  _RegisterTest('ACBrPIXCD.Schemas.Pix', TTestCobrancaImediataComSaquePIX3);
  _RegisterTest('ACBrPIXCD.Schemas.Pix', TTestCobrancaGerada);
  _RegisterTest('ACBrPIXCD.Schemas.Problema', TTestProblema);
  _RegisterTest('ACBrPIXCD.Schemas.Pix', TTestPixConsultados);
  _RegisterTest('ACBrPIXCD.Schemas.Cob', TTestCobsConsultadas);
  _RegisterTest('ACBrPIXCD.Schemas.Cob', TTestCobRevisada);
  _RegisterTest('ACBrPIXCD.Schemas.CobV', TTestCobVSolicitada);
  _RegisterTest('ACBrPIXCD.Schemas.CobV', TTestCobVGerada);
  _RegisterTest('ACBrPIXCD.Schemas.CobV', TTestCobsVConsultadas);
  _RegisterTest('ACBrPIXCD.Schemas.CobV', TTestCobVRevisada);
  _RegisterTest('ACBrPIXCD.Schemas.Lote', TTestLoteCobVBody);
  _RegisterTest('ACBrPIXCD.Schemas.Lote', TTestLoteCobVBodyRevisado);
  _RegisterTest('ACBrPIXCD.Schemas.Lote', TTestLoteCobVConsultado);
  _RegisterTest('ACBrPIXCD.Schemas.Lote', TTestLotesCobVConsultados);

end.

