unit ACBrPIXSchemasTeste;
 
{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrPIXSchemas,
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

implementation

uses
  ACBrUtil;

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


end.

