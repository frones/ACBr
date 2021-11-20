unit ACBrPIXSchemasTeste;
 
{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrPIXBase, ACBrPIXSchemasCobranca, ACBrPIXQRCodeEstatico,
  ACBrPIXSchemasProblema, ACBrPIXSchemasPixConsultados,
  {$ifdef FPC}
   fpcunit, testutils, testregistry
  {$else}
   {$IFDEF DUNITX}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility
   {$ELSE}
    TestFramework
   {$ENDIF}
  {$endif};

type

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

implementation

uses
  DateUtils,
  ACBrUtil;

{ TTestCobrancaImediataExemplo1 }

procedure TTestCobrancaImediataExemplo1.SetUp;
begin
  inherited;
  fACBrPixCob := TACBrPIXCobSolicitada.Create;

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
  pc := TACBrPIXCobSolicitada.Create;
  try
    pc.AsJSON := s;
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
  fACBrPixCob := TACBrPIXCobSolicitada.Create;

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
  pc := TACBrPIXCobSolicitada.Create;
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
  fACBrPixCob := TACBrPIXCobSolicitada.Create;

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
  pc := TACBrPIXCobSolicitada.Create;
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
  fACBrPixCob := TACBrPIXCobSolicitada.Create;

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
  pc := TACBrPIXCobSolicitada.Create;
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
  fACBrPixCobGerada := TACBrPIXCobGerada.Create;
  fJSON := ACBrStr(
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
  CheckEquals(fACBrPixCobGerada.calendario.criacao, EncodeDateTime(2020,09,09,20,15,0,358));
  CheckEquals(fACBrPixCobGerada.calendario.expiracao, 3600);
  CheckEquals(fACBrPixCobGerada.txId, '33beb661beda44a8928fef47dbeb2dc5');
  CheckEquals(fACBrPixCobGerada.revisao, 0);
  CheckEquals(fACBrPixCobGerada.loc.id, '1004');
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
  cg := TACBrPIXCobGerada.Create;
  try
    cg.AsJSON := s;
    CheckEquals(fACBrPixCobGerada.calendario.criacao, cg.calendario.criacao);
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
  fQREstatico.ChavePix := '123e4567-e12b-12d1-a456-426655440000';
  fQREstatico.NomeRecebedor := 'Fulano de Tal';
  fQREstatico.CidadeRecebedor := 'BRASILIA';

  CheckEquals(fQREstatico.QRCode, fQRStr);
end;

{ TTestProblema }

procedure TTestProblema.SetUp;
begin
  inherited SetUp;
  fACBrPixProblema := TACBrPIXProblema.Create;
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
  pb := TACBrPIXProblema.Create;
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

procedure TTestQRCodeEstatico.AtribuirQRCodeEVerificarDados;
begin
  fQREstatico.QRCode := fQRStr;
  CheckEquals(fQREstatico.ChavePix, '123e4567-e12b-12d1-a456-426655440000');
  CheckEquals(fQREstatico.NomeRecebedor, 'Fulano de Tal');
  CheckEquals(fQREstatico.CidadeRecebedor, 'BRASILIA');
  CheckEquals(fQREstatico.QRCode, fQRStr);
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
  fACBrPixConsultados := TACBrPIXConsultados.Create;
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
  CheckEquals(fACBrPixConsultados.pix[2].componentesValor.saque.prestadorDeServicoDeSaque, 12345678);
end;

procedure TTestPixConsultados.AtribuirLerReatribuirEComparar;
var
  pc: TACBrPIXConsultados;
  s: String;
begin
  fACBrPixConsultados.AsJSON := fJSON;
  s := fACBrPixConsultados.AsJSON;
  pc := TACBrPIXConsultados.Create;
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
    CheckEquals(fACBrPixConsultados.pix[2].componentesValor.saque.prestadorDeServicoDeSaque, pc.pix[2].componentesValor.saque.prestadorDeServicoDeSaque);
  finally
    pc.Free;
  end;
end;


procedure _RegisterTest(ATesteName: String; ATestClass: TClass);
begin
  {$IfDef DUNITX}
   TDUnitX.RegisterTestFixture( ATestClass, ATesteName );
  {$ELSE}
   RegisterTest(ATesteName, TTestCaseClass(ATestClass){$IfNDef FPC}.Suite{$EndIf} );
  {$EndIf}
end;

initialization

  _RegisterTest('ACBrPIXCD.Schemas', TTestCobrancaImediataExemplo1);
  _RegisterTest('ACBrPIXCD.Schemas', TTestCobrancaImediataComSaquePIX);
  _RegisterTest('ACBrPIXCD.Schemas', TTestCobrancaImediataComSaquePIX2);
  _RegisterTest('ACBrPIXCD.Schemas', TTestCobrancaImediataComSaquePIX3);
  _RegisterTest('ACBrPIXCD.Schemas', TTestCobrancaGerada);
  _RegisterTest('ACBrPIXCD.Schemas', TTestQRCodeEstatico);
  _RegisterTest('ACBrPIXCD.Schemas', TTestProblema);
  _RegisterTest('ACBrPIXCD.Schemas', TTestPixConsultados);

end.

