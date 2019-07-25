unit ACBrValidadorTest;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  {$ifdef FPC}
  fpcunit, testutils, testregistry,
  {$else}
  TestFramework,
  {$endif}
  SysUtils, ACBrValidador;

type

  { TTestCaseACBrValidadorCPF }

  TTestCaseACBrValidadorCPF = class(TTestCase)
  private
    fACBrValidador : TACBrValidador;
    function MsgErroCPF: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Valido;
    procedure ValidoComSeparadores;
    procedure Invalido;
    procedure NumerosSequenciais;
    procedure MenorOnzeDigitos;
    procedure MaiorOnzeDigitos;
    procedure ComLetras;
    procedure Formatar;
  end;

  { TTestCaseACBrValidadorCNPJ }

  TTestCaseACBrValidadorCNPJ = class(TTestCase)
  private
    fACBrValidador : TACBrValidador;
    function MsgErroCNPJ: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Valido;
    procedure ValidoComSeparadores;
    procedure Invalido;
    procedure NumeroComZeros;
    procedure MenorQuatorzeDigitos;
    procedure MaiorQuatorzeDigitos;
    procedure ComLetras;
    procedure Formatar;
  end;

  { TTestCaseACBrValidadorUF }

  TTestCaseACBrValidadorUF = class(TTestCase)
  private
    fACBrValidador : TACBrValidador;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Validos;
    procedure Invalido;
  end;

  { TTestCaseACBrValidadorIE }

  TTestCaseACBrValidadorIE = class(TTestCase)
  private
    fACBrValidador : TACBrValidador;
    function MsgErroIE: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ValidoAC ;
    procedure InvalidoAC ;
    procedure FormatarAC;
    procedure ValidoAL ;
    procedure InvalidoAL ;
    procedure FormatarAL;
    procedure ValidoAP ;
    procedure InvalidoAP ;
    procedure FormatarAP;
    procedure ValidoAM ;
    procedure InvalidoAM ;
    procedure FormatarAM;
    procedure ValidoBA ;
    procedure InvalidoBA ;
    procedure FormatarBA;
    procedure ValidoCE ;
    procedure InvalidoCE ;
    procedure FormatarCE;
    procedure ValidoDF ;
    procedure InvalidoDF ;
    procedure FormatarDF;
    procedure ValidoES ;
    procedure InvalidoES ;
    procedure FormatarES;
    procedure ValidoGO ;
    procedure InvalidoGO ;
    procedure FormatarGO;
    procedure ValidoMA ;
    procedure InvalidoMA ;
    procedure FormatarMA;
    procedure ValidoMT ;
    procedure InvalidoMT ;
    procedure FormatarMT;
    procedure ValidoMS ;
    procedure InvalidoMS ;
    procedure FormatarMS;
    procedure ValidoMG ;
    procedure InvalidoMG ;
    procedure FormatarMG;
    procedure ValidoPA ;
    procedure InvalidoPA ;
    procedure FormatarPA;
    procedure ValidoPB ;
    procedure InvalidoPB ;
    procedure FormatarPB;
    procedure ValidoPR ;
    procedure InvalidoPR ;
    procedure FormatarPR;
    procedure ValidoPE ;
    procedure InvalidoPE ;
    procedure FormatarPE;
    procedure ValidoPI ;
    procedure InvalidoPI ;
    procedure FormatarPI;
    procedure ValidoRJ ;
    procedure InvalidoRJ ;
    procedure FormatarRJ;
    procedure ValidoRN ;
    procedure InvalidoRN ;
    procedure FormatarRN;
    procedure ValidoRS ;
    procedure InvalidoRS ;
    procedure FormatarRS;
    procedure ValidoRO ;
    procedure InvalidoRO ;
    procedure FormatarRO;
    procedure ValidoRR ;
    procedure InvalidoRR ;
    procedure FormatarRR;
    procedure ValidoSC ;
    procedure InvalidoSC ;
    procedure FormatarSC;
    procedure ValidoSP ;
    procedure InvalidoSP ;
    procedure FormatarSP;
    procedure ValidoSE ;
    procedure InvalidoSE ;
    procedure FormatarSE;
    procedure ValidoTO ;
    procedure InvalidoTO ;
    procedure FormatarTO;
  end;

  {TTestCaseACBrValidadorTelefone}

  TTestCaseACBrValidadorTelefone = class(TTestCase)
  published
    procedure FormatarVazio;
    procedure FormatarZeros;
    procedure FormatarSemDDD;
    procedure FormatarComDDD;
    procedure FormatarComDDDPadrao;
    procedure FormatarSemDDD9Dig;
    procedure FormatarComDDD9Dig;
    procedure Formatar0300;
    procedure Formatar0500;
    procedure Formatar0800;
    procedure Formatar0900;
    procedure Formatar55ComDDD;
    procedure Formatar55ComDDD9Dig;
  end;

  { TTestCaseACBrValidadorCEP }

  TTestCaseACBrValidadorCEP = class(TTestCase)
  published
    procedure VerificarInvalidoSP;
    procedure VerificarValidoSP;
    procedure VerificarValidoSemExtensaoSP;
    procedure FormatarInteger;
    procedure FormatarIntegerSemExtensao;
    procedure VerificarIntegerValidoSP;
    procedure ComTraco;
    procedure ComTracoNoLugarErrado;
    procedure ComLetras;
    procedure FormatarVazio;
    procedure FormatarZerosAEsquerda;
    procedure FormatarMenosDeCincoDigitos;
    procedure FormatarCincoDigitos;
    procedure FormatarMenosDeOitoEMaisDeCincoDigitos;
    procedure FormatarMaisDeOitoDigitos;
  end;

  { TTestCaseACBrValidadorEmail }

  TTestCaseACBrValidadorEmail = class(TTestCase)
  published
    procedure ValidarEmailsValidos;
    procedure EmailInvalidoComEspacos;
    procedure EmailInvalidoComecandoComPonto;
    procedure EmailInvalidoComecandoComArroba;
    procedure EmailInvalidoComDoisPontosSeguidos;
    procedure EmailInvalidoArrobaComPonto;
    procedure EmailInvalidoTerminandoComPonto;
    procedure EmailInvalidoTerminandoComArroba;
    procedure EmailInvalidoComCarecteresEspeciais;
    procedure ValidarListaEmailsValidos;
    procedure ValidarListaEmailsInvalidos;
    procedure ValidarListaEmailsMisturandoDelimitadores;
  end;


  { TTestCaseACBrValidadorPlaca }

  TTestCaseACBrValidadorPlaca = class(TTestCase)
  private
    fACBrValidador : TACBrValidador;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FormatarPlaca;
    procedure PlacasValidas;
    procedure PlacasInvalidas;
  end;


implementation

{ TTestCaseACBrValidadorCEP }

procedure TTestCaseACBrValidadorCEP.VerificarInvalidoSP;
begin
  CheckNotEquals('', ACBrValidador.ValidarCEP('92260001', 'SP'));
end;

procedure TTestCaseACBrValidadorCEP.VerificarValidoSP;
begin
  CheckEquals('', ACBrValidador.ValidarCEP('02260001', 'SP'));
end;

procedure TTestCaseACBrValidadorCEP.VerificarValidoSemExtensaoSP;
begin
  CheckEquals('', ACBrValidador.ValidarCEP(18270, 'SP'));
end;

procedure TTestCaseACBrValidadorCEP.FormatarInteger;
var
  ACep: Integer;
begin
  ACep := 2260001;
  CheckEquals('02260-001', ACBrValidador.FormatarCEP(ACep));
end;

procedure TTestCaseACBrValidadorCEP.FormatarIntegerSemExtensao;
var
  ACep: Integer;
begin
  ACep := 2260;
  CheckEquals('02260-000', ACBrValidador.FormatarCEP(ACep));
end;

procedure TTestCaseACBrValidadorCEP.VerificarIntegerValidoSP;
var
  ACep: Integer;
begin
  ACep := 2260001;
  CheckEquals('', ACBrValidador.ValidarCEP(ACep, 'SP'));
end;

procedure TTestCaseACBrValidadorCEP.ComTraco;
begin
  CheckEquals('', ACBrValidador.ValidarCEP('02260-001', 'SP'));
end;

procedure TTestCaseACBrValidadorCEP.ComTracoNoLugarErrado;
begin
  CheckEquals('', ACBrValidador.ValidarCEP('0226-0001', 'SP'));
end;

procedure TTestCaseACBrValidadorCEP.ComLetras;
begin
  CheckNotEquals('', ACBrValidador.ValidarCEP('ABCDEFGHIJ', 'SP'));
end;

procedure TTestCaseACBrValidadorCEP.FormatarVazio;
begin
  CheckEquals('00000-000', ACBrValidador.FormatarCEP(''));
end;

procedure TTestCaseACBrValidadorCEP.FormatarZerosAEsquerda;
begin
  CheckEquals('02260-001', ACBrValidador.FormatarCEP('02260001'));
end;

procedure TTestCaseACBrValidadorCEP.FormatarMenosDeCincoDigitos;
begin
  CheckEquals('02260-000', ACBrValidador.FormatarCEP('2260'));
end;

procedure TTestCaseACBrValidadorCEP.FormatarCincoDigitos;
begin
  CheckEquals('18270-000', ACBrValidador.FormatarCEP('18270'));
end;

procedure TTestCaseACBrValidadorCEP.FormatarMenosDeOitoEMaisDeCincoDigitos;
begin
  CheckEquals('02260-001', ACBrValidador.FormatarCEP('2260001'));
end;

procedure TTestCaseACBrValidadorCEP.FormatarMaisDeOitoDigitos;
begin
  CheckEquals('12345-678', ACBrValidador.FormatarCEP('123456789'));
end;

{ TTestCaseACBrValidadorTelefone }

procedure TTestCaseACBrValidadorTelefone.FormatarVazio;
begin
  CheckEquals('', ACBrValidador.FormatarFone(''));
end;

procedure TTestCaseACBrValidadorTelefone.FormatarZeros;
begin
  CheckEquals('', ACBrValidador.FormatarFone('0000000000', ''));
end;

procedure TTestCaseACBrValidadorTelefone.FormatarSemDDD;
begin
  CheckEquals('3322-0000', ACBrValidador.FormatarFone('33220000', ''));
  CheckEquals('4004-1234', ACBrValidador.FormatarFone('40041234', ''));
end;

procedure TTestCaseACBrValidadorTelefone.FormatarComDDD;
begin
  CheckEquals('(15)3322-0000', ACBrValidador.FormatarFone('01533220000', ''));
  CheckEquals('(15)3322-0000', ACBrValidador.FormatarFone('1533220000', ''));
  CheckEquals('(15)4004-1234', ACBrValidador.FormatarFone('01540041234', ''));
  CheckEquals('(15)4004-1235', ACBrValidador.FormatarFone('1540041235', ''));
end;

procedure TTestCaseACBrValidadorTelefone.FormatarComDDDPadrao;
begin
  CheckEquals('(15)3322-0000', ACBrValidador.FormatarFone('33220000', '15'));
  CheckEquals('(15)4422-0000', ACBrValidador.FormatarFone('044220000', '15'));
end;

procedure TTestCaseACBrValidadorTelefone.FormatarSemDDD9Dig;
begin
  CheckEquals('99701-2345', ACBrValidador.FormatarFone('997012345', ''));
end;

procedure TTestCaseACBrValidadorTelefone.FormatarComDDD9Dig;
begin
  CheckEquals('(15)99701-2345', ACBrValidador.FormatarFone('015997012345',''));
  CheckEquals('(15)99701-2346', ACBrValidador.FormatarFone('15997012346',''));
  CheckEquals('(15)99701-2347', ACBrValidador.FormatarFone('997012347','15'));
end;

procedure TTestCaseACBrValidadorTelefone.Formatar0300;
begin
  CheckEquals('0300-123-4567', ACBrValidador.FormatarFone('03001234567', ''));
end;

procedure TTestCaseACBrValidadorTelefone.Formatar0500;
begin
  CheckEquals('0500-123-4567', ACBrValidador.FormatarFone('05001234567', ''));
end;

procedure TTestCaseACBrValidadorTelefone.Formatar0800;
begin
  CheckEquals('0800-123-4567', ACBrValidador.FormatarFone('08001234567', ''));
end;

procedure TTestCaseACBrValidadorTelefone.Formatar0900;
begin
  CheckEquals('0900-123-4567', ACBrValidador.FormatarFone('09001234567', ''));
end;

procedure TTestCaseACBrValidadorTelefone.Formatar55ComDDD;
begin
  CheckEquals('+55(11)3322-0000', ACBrValidador.FormatarFone('551133220000', ''));
  CheckEquals('+55(11)3323-0000', ACBrValidador.FormatarFone('5501133230000', ''));
  CheckEquals('+55(11)4004-1234', ACBrValidador.FormatarFone('551140041234', ''));
  CheckEquals('+55(11)4004-1235', ACBrValidador.FormatarFone('5501140041235', ''));
end;

procedure TTestCaseACBrValidadorTelefone.Formatar55ComDDD9Dig;
begin
  CheckEquals('+55(11)99922-0000', ACBrValidador.FormatarFone('5511999220000', ''));
  CheckEquals('+55(11)99923-0000', ACBrValidador.FormatarFone('55011999230000', ''));
end;


{ TTestCaseACBrValidadorIE }

function TTestCaseACBrValidadorIE.MsgErroIE: String;
begin
  Result := fACBrValidador.MsgErro + ' - '+fACBrValidador.Documento;
end;

procedure TTestCaseACBrValidadorIE.SetUp;
begin
  fACBrValidador := TACBrValidador.Create(nil);
  fACBrValidador.TipoDocto := docInscEst;
  fACBrValidador.ExibeDigitoCorreto := True;
end;

procedure TTestCaseACBrValidadorIE.TearDown;
begin
  FreeAndNil( fACBrValidador );
end;

procedure TTestCaseACBrValidadorIE.ValidoAC;
begin
  fACBrValidador.Complemento := 'AC';
  fACBrValidador.Documento := '01.004.823/001-12';  // 13 dígitos
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '013456784';          // 9 dígitos
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoAC;
begin
  fACBrValidador.Complemento := 'AC';
  fACBrValidador.Documento := '';             // Testando Vazio apenas 1 vez
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '99999';        // Testando menos dígitos apenas 1 vez
  fACBrValidador.AjustarTamanho := True;
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '01.004.823/001-99';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarAC;
begin
  fACBrValidador.Complemento := 'AC';
  fACBrValidador.Documento := '0100482300112';
  CheckEquals('01.004.823/001-12', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoAL;
begin
  fACBrValidador.Complemento := 'AL';
  fACBrValidador.Documento := '240123450';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoAL;
begin
  fACBrValidador.Complemento := 'AL';
  fACBrValidador.Documento := '240123456';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarAL;
begin
  fACBrValidador.Complemento := 'AL';
  fACBrValidador.Documento := '240123450';
  CheckEquals('240123450', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoAP;
begin
  fACBrValidador.Complemento := 'AP';
  fACBrValidador.Documento := '030123459';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '030170011';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '030190225';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '030190231';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoAP;
begin
  fACBrValidador.Complemento := 'AP';
  fACBrValidador.Documento := '123456789';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarAP;
begin
  fACBrValidador.Complemento := 'AP';
  fACBrValidador.Documento := '030123459';
  CheckEquals('030123459', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoAM;
begin
  fACBrValidador.Complemento := 'AM';
  fACBrValidador.Documento := '123123127';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoAM;
begin
  fACBrValidador.Complemento := 'AM';
  fACBrValidador.Documento := '123123123';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarAM;
begin
  fACBrValidador.Complemento := 'AM';
  fACBrValidador.Documento := '123123127';
  CheckEquals('12.312.312-7', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoBA;
begin
  fACBrValidador.Complemento := 'BA';
  fACBrValidador.Documento := '123456-63';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Complemento := 'BA';
  fACBrValidador.Documento := '173456-13';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoBA;
begin
  fACBrValidador.Complemento := 'BA';
  fACBrValidador.Documento := '123456-78';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarBA;
begin
  fACBrValidador.Complemento := 'BA';
  fACBrValidador.Documento := '12345663';
  CheckEquals('0123456-63', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoCE;
begin
  fACBrValidador.Complemento := 'CE';
  fACBrValidador.Documento := '023456787';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoCE;
begin
  fACBrValidador.Complemento := 'CE';
  fACBrValidador.Documento := '123456789';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarCE;
begin
  fACBrValidador.Complemento := 'CE';
  fACBrValidador.Documento := '023456787';
  CheckEquals('02345678-7', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoDF;
begin
  fACBrValidador.Complemento := 'DF';
  fACBrValidador.Documento := '0734567890103';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoDF;
begin
  fACBrValidador.Complemento := 'DF';
  fACBrValidador.Documento := '12345678901';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);  // Tamanho inválido
  fACBrValidador.Documento := '1234567890123';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarDF;
begin
  fACBrValidador.Complemento := 'DF';
  fACBrValidador.Documento := '0734567890103';
  CheckEquals('07345678901-03', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoES;
begin
  fACBrValidador.Complemento := 'ES';
  fACBrValidador.Documento := '123123127';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoES;
begin
  fACBrValidador.Complemento := 'ES';
  fACBrValidador.Documento := '123123123';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarES;
begin
  fACBrValidador.Complemento := 'ES';
  fACBrValidador.Documento := '123123127';
  CheckEquals('123123127', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoGO;
begin
  fACBrValidador.Complemento := 'GO';
  fACBrValidador.Documento := '10.987.654-7';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoGO;
begin
  fACBrValidador.Complemento := 'GO';
  fACBrValidador.Documento := '12.312.312-3';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarGO;
begin
  fACBrValidador.Complemento := 'GO';
  fACBrValidador.Documento := '109876547';
  CheckEquals('10.987.654-7', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoMA;
begin
  fACBrValidador.Complemento := 'MA';
  fACBrValidador.Documento := '120000385';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoMA;
begin
  fACBrValidador.Complemento := 'MA';
  fACBrValidador.Documento := '123123123';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarMA;
begin
  fACBrValidador.Complemento := 'MA';
  fACBrValidador.Documento := '120000385';
  CheckEquals('120000385', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoMT;
begin
  fACBrValidador.Complemento := 'MT';
  fACBrValidador.Documento := '0013000001-9';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoMT;
begin
  fACBrValidador.Complemento := 'MT';
  fACBrValidador.Documento := '1234567890-1';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarMT;
begin
  fACBrValidador.Complemento := 'MT';
  fACBrValidador.Documento := '130000019';
  CheckEquals('0013000001-9', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoMS;
begin
  fACBrValidador.Complemento := 'MS';
  fACBrValidador.Documento := '28.312.312-5';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoMS;
begin
  fACBrValidador.Complemento := 'MS';
  fACBrValidador.Documento := '123123123';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarMS;
begin
  fACBrValidador.Complemento := 'MS';
  fACBrValidador.Documento := '283123125';
  CheckEquals('28.312.312-5', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoMG;
begin
  fACBrValidador.Complemento := 'MG';
  fACBrValidador.Documento := '062.307.904/0081';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoMG;
begin
  fACBrValidador.Complemento := 'MG';
  fACBrValidador.Documento := '123.123.123/9999';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarMG;
begin
  fACBrValidador.Complemento := 'MG';
  fACBrValidador.Documento := '0623079040081';
  CheckEquals('062.307.904/0081', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoPA;
begin
  fACBrValidador.Complemento := 'PA';
  fACBrValidador.Documento := '15999999-5';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoPA;
begin
  fACBrValidador.Complemento := 'PA';
  fACBrValidador.Documento := '15999999-9';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarPA;
begin
  fACBrValidador.Complemento := 'PA';
  fACBrValidador.Documento := '159999995';
  CheckEquals('15-999999-5', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoPB;
begin
  fACBrValidador.Complemento := 'PB';
  fACBrValidador.Documento := '16000001-7';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoPB;
begin
  fACBrValidador.Complemento := 'PB';
  fACBrValidador.Documento := '06000001-9';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarPB;
begin
  fACBrValidador.Complemento := 'PB';
  fACBrValidador.Documento := '160000017';
  CheckEquals('16000001-7', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoPR;
begin
  fACBrValidador.Complemento := 'PR';
  fACBrValidador.Documento := '123.45678-50';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoPR;
begin
  fACBrValidador.Complemento := 'PR';
  fACBrValidador.Documento := '123.45678-99';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarPR;
begin
  fACBrValidador.Complemento := 'PR';
  fACBrValidador.Documento := '1234567850';
  CheckEquals('123.45678-50', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoPE;
begin
  fACBrValidador.Complemento := 'PE';
  fACBrValidador.Documento := '0321418-40';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '18.1.001.0000004-9';    // Antigo formato
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoPE;
begin
  fACBrValidador.Complemento := 'PE';
  fACBrValidador.Documento := '0321418-99';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '18.1.001.0000004-0';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarPE;
begin
  fACBrValidador.Complemento := 'PE';
  fACBrValidador.Documento := '032141899';
  CheckEquals('0321418-99', fACBrValidador.Formatar);
  fACBrValidador.Documento := '18100100000040';
  CheckEquals('18.1.001.0000004-0', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoPI;
begin
  fACBrValidador.Complemento := 'PI';
  fACBrValidador.Documento := '192345672';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoPI;
begin
  fACBrValidador.Complemento := 'PI';
  fACBrValidador.Documento := '012345678';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarPI;
begin
  fACBrValidador.Complemento := 'PI';
  fACBrValidador.Documento := '192345672';
  CheckEquals('192345672', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoRJ;
begin
  fACBrValidador.Complemento := 'RJ';
  fACBrValidador.Documento := '12.123.12-4';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoRJ;
begin
  fACBrValidador.Complemento := 'RJ';
  fACBrValidador.Documento := '12.123.12-9';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarRJ;
begin
  fACBrValidador.Complemento := 'RJ';
  fACBrValidador.Documento := '12123124';
  CheckEquals('12.123.12-4', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoRN;
begin
  fACBrValidador.Complemento := 'RN';
  fACBrValidador.Documento := '20.040.040-1';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '20.0.040.040-0';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoRN;
begin
  fACBrValidador.Complemento := 'RN';
  fACBrValidador.Documento := '20.040.040-9';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '20.0.040.040-9';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarRN;
begin
  fACBrValidador.Complemento := 'RN';
  fACBrValidador.Documento := '200400401';
  CheckEquals('20.040.040-1', fACBrValidador.Formatar);
  fACBrValidador.Documento := '2000400400';
  CheckEquals('20.0.040.040-0', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoRS;
begin
  fACBrValidador.Complemento := 'RS';
  fACBrValidador.Documento := '224/3658792';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoRS;
begin
  fACBrValidador.Complemento := 'RS';
  fACBrValidador.Documento := '224/1234567';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarRS;
begin
  fACBrValidador.Complemento := 'RS';
  fACBrValidador.Documento := '2243658792';
  CheckEquals('224/3658792', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoRO;
begin
  fACBrValidador.Complemento := 'RO';
  fACBrValidador.Documento := '101.62521-3';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '0000000062521-3';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoRO;
begin
  fACBrValidador.Complemento := 'RO';
  fACBrValidador.Documento := '101.12345-6';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '1234567890521-3';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarRO;
begin
  fACBrValidador.Complemento := 'RO';
  fACBrValidador.Documento := '101625213';
  CheckEquals('101.62521-3', fACBrValidador.Formatar);
  fACBrValidador.Documento := '0000000062521-3';
  CheckEquals('0000000062521-3', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoRR;
begin
  fACBrValidador.Complemento := 'RR';
  fACBrValidador.Documento := '24008266-8';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoRR;
begin
  fACBrValidador.Complemento := 'RR';
  fACBrValidador.Documento := '12345678-8';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarRR;
begin
  fACBrValidador.Complemento := 'RR';
  fACBrValidador.Documento := '240082668';
  CheckEquals('24008266-8', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoSC;
begin
  fACBrValidador.Complemento := 'SC';
  fACBrValidador.Documento := '251.040.852';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoSC;
begin
  fACBrValidador.Complemento := 'SC';
  fACBrValidador.Documento := '123.123.123';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarSC;
begin
  fACBrValidador.Complemento := 'SC';
  fACBrValidador.Documento := '251040852';
  CheckEquals('251.040.852', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoSP;
begin
  fACBrValidador.Complemento := 'SP';
  fACBrValidador.Documento := '110.042.490.114';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := 'P-01100424.3/002';  // Produtor Rural
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoSP;
begin
  fACBrValidador.Complemento := 'SP';
  fACBrValidador.Documento := '123.123.123.123';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := 'P-12345678.9/002';  // Produtor Rural
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarSP;
begin
  fACBrValidador.Complemento := 'SP';
  fACBrValidador.Documento := '110042490114';
  CheckEquals('110.042.490.114', fACBrValidador.Formatar);
  fACBrValidador.Documento := 'P011004243123';
  CheckEquals('P-01100424.3/123', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoSE;
begin
  fACBrValidador.Complemento := 'SE';
  fACBrValidador.Documento := '27123456-3';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoSE;
begin
  fACBrValidador.Complemento := 'SE';
  fACBrValidador.Documento := '12312312-3';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarSE;
begin
  fACBrValidador.Complemento := 'SE';
  fACBrValidador.Documento := '271234563';
  CheckEquals('27.123.456-3', fACBrValidador.Formatar);
end;

procedure TTestCaseACBrValidadorIE.ValidoTO;
begin
  fACBrValidador.Complemento := 'TO';
  fACBrValidador.Documento := '01.022.783-0';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '29.01.022783-6';
  CheckTrue(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.InvalidoTO;
begin
  fACBrValidador.Complemento := 'TO';
  fACBrValidador.Documento := '12.123.123-9';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
  fACBrValidador.Documento := '12.34.567890-6';
  CheckFalse(fACBrValidador.Validar, MsgErroIE);
end;

procedure TTestCaseACBrValidadorIE.FormatarTO;
begin
  fACBrValidador.Complemento := 'TO';
  fACBrValidador.Documento := '010227830';
  CheckEquals('01.022.783-0', fACBrValidador.Formatar);
  fACBrValidador.Documento := '29010227836';
  CheckEquals('29.01.022783-6', fACBrValidador.Formatar);
end;

{ TTestCaseACBrValidadorCPF }

function TTestCaseACBrValidadorCPF.MsgErroCPF: String;
begin
  Result := fACBrValidador.MsgErro + ' - '+fACBrValidador.Documento;
end;

procedure TTestCaseACBrValidadorCPF.SetUp;
begin
  fACBrValidador := TACBrValidador.Create(nil);
  fACBrValidador.TipoDocto := docCPF;
  fACBrValidador.ExibeDigitoCorreto := True;
end;

procedure TTestCaseACBrValidadorCPF.TearDown;
begin
  FreeAndNil( fACBrValidador );
end;

procedure TTestCaseACBrValidadorCPF.Valido;
begin
  fACBrValidador.Documento := '12345678909';
  CheckTrue(fACBrValidador.Validar, MsgErroCPF);
end;

procedure TTestCaseACBrValidadorCPF.ValidoComSeparadores;
begin
  fACBrValidador.Documento := '123.456.789-09';
  CheckTrue(fACBrValidador.Validar, MsgErroCPF);
  fACBrValidador.Documento := '191';
  fACBrValidador.AjustarTamanho := True;
  CheckTrue(fACBrValidador.Validar, MsgErroCPF);
end;

procedure TTestCaseACBrValidadorCPF.Invalido;
begin
  fACBrValidador.Documento := '12345678901';
  CheckFalse(fACBrValidador.Validar, MsgErroCPF)
end;

procedure TTestCaseACBrValidadorCPF.NumerosSequenciais;
var
  I: Char;
begin
  For I := '1' to '9' do
  begin
    fACBrValidador.Documento := StringOfChar(I,11);
    CheckFalse(fACBrValidador.Validar, MsgErroCPF);
  end;
end;

procedure TTestCaseACBrValidadorCPF.MenorOnzeDigitos;
begin
  fACBrValidador.Documento := '123456789';
  fACBrValidador.AjustarTamanho := False;
  CheckFalse(fACBrValidador.Validar, MsgErroCPF)
end;

procedure TTestCaseACBrValidadorCPF.MaiorOnzeDigitos;
begin
  fACBrValidador.Documento := '1234567890123';
  fACBrValidador.AjustarTamanho := False;
  CheckFalse(fACBrValidador.Validar, MsgErroCPF)
end;

procedure TTestCaseACBrValidadorCPF.ComLetras;
begin
  fACBrValidador.Documento := '123456789AB';
  CheckFalse(fACBrValidador.Validar, MsgErroCPF)
end;

procedure TTestCaseACBrValidadorCPF.Formatar;
begin
  fACBrValidador.Documento := '191';
  fACBrValidador.AjustarTamanho := True;
  CheckEquals('000.000.001-91', fACBrValidador.Formatar);
  fACBrValidador.Documento := '12345678909';
  CheckEquals('123.456.789-09', fACBrValidador.Formatar);
end;


{ TTestCaseACBrValidadorCNPJ }

function TTestCaseACBrValidadorCNPJ.MsgErroCNPJ: String;
begin
  Result := fACBrValidador.MsgErro + ' - '+fACBrValidador.Documento;
end;

procedure TTestCaseACBrValidadorCNPJ.SetUp;
begin
  fACBrValidador := TACBrValidador.Create(nil);
  fACBrValidador.TipoDocto := docCNPJ;
  fACBrValidador.ExibeDigitoCorreto := True;
end;

procedure TTestCaseACBrValidadorCNPJ.TearDown;
begin
  FreeAndNil( fACBrValidador );
end;

procedure TTestCaseACBrValidadorCNPJ.Valido;
begin
  fACBrValidador.Documento := '12345678000195';
  CheckTrue(fACBrValidador.Validar, MsgErroCNPJ);
  fACBrValidador.Documento := '191';
  fACBrValidador.AjustarTamanho := True;
  CheckTrue(fACBrValidador.Validar, MsgErroCNPJ);
end;

procedure TTestCaseACBrValidadorCNPJ.ValidoComSeparadores;
begin
  fACBrValidador.Documento := '12.345.678/0001-95';
  CheckTrue(fACBrValidador.Validar, MsgErroCNPJ);
end;

procedure TTestCaseACBrValidadorCNPJ.Invalido;
begin
  fACBrValidador.Documento := '12345678901234';
  CheckFalse(fACBrValidador.Validar, MsgErroCNPJ);
end;

procedure TTestCaseACBrValidadorCNPJ.NumeroComZeros;
begin
  fACBrValidador.Documento := StringOfChar('0',14);
  CheckFalse(fACBrValidador.Validar, MsgErroCNPJ);
end;

procedure TTestCaseACBrValidadorCNPJ.MenorQuatorzeDigitos;
begin
  fACBrValidador.Documento := '1234567890';
  fACBrValidador.AjustarTamanho := False;
  CheckFalse(fACBrValidador.Validar, MsgErroCNPJ);
end;

procedure TTestCaseACBrValidadorCNPJ.MaiorQuatorzeDigitos;
begin
  fACBrValidador.Documento := '123456789012345';
  fACBrValidador.AjustarTamanho := False;
  CheckFalse(fACBrValidador.Validar, MsgErroCNPJ);
end;

procedure TTestCaseACBrValidadorCNPJ.ComLetras;
begin
  fACBrValidador.Documento := '1234567890ABCD';
  CheckFalse(fACBrValidador.Validar, MsgErroCNPJ);
end;

procedure TTestCaseACBrValidadorCNPJ.Formatar;
begin
  fACBrValidador.Documento := '191';
  fACBrValidador.AjustarTamanho := True;
  CheckEquals('00.000.000/0001-91', fACBrValidador.Formatar);
  fACBrValidador.Documento := '12345678000195';
  CheckEquals('12.345.678/0001-95', fACBrValidador.Formatar);
end;

{ TTestCaseACBrValidadorUF }

procedure TTestCaseACBrValidadorUF.SetUp;
begin
  fACBrValidador := TACBrValidador.Create(nil);
  fACBrValidador.TipoDocto := docUF;
end;

procedure TTestCaseACBrValidadorUF.TearDown;
begin
  FreeAndNil(fACBrValidador);
end;

procedure TTestCaseACBrValidadorUF.Validos;
var
  SL: TStringList;
  I: Integer;
  aUF: String;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.DelimitedText := cUFsValidas;

    For I := 0 to SL.Count-1 do
    begin
      aUF := Trim(SL[I]);
      if aUF <> '' then
      begin
        fACBrValidador.Documento := aUF;
        CheckTrue(fACBrValidador.Validar, fACBrValidador.MsgErro);
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure TTestCaseACBrValidadorUF.Invalido;
begin
  fACBrValidador.Documento := '';
  CheckFalse(fACBrValidador.Validar, fACBrValidador.MsgErro);
  fACBrValidador.Documento := 'X';
  CheckFalse(fACBrValidador.Validar, fACBrValidador.MsgErro);
  fACBrValidador.Documento := 'XX';
  CheckFalse(fACBrValidador.Validar, fACBrValidador.MsgErro);
  fACBrValidador.Documento := 'XXX';
  CheckFalse(fACBrValidador.Validar, fACBrValidador.MsgErro);
end;

{ TTestCaseACBrValidadorEmail }

procedure TTestCaseACBrValidadorEmail.EmailInvalidoComEspacos;
begin
  CheckNotEquals('', ValidarEmail('nome com espaco@hotmail.com'));
end;

procedure TTestCaseACBrValidadorEmail.EmailInvalidoComecandoComPonto;
begin
  CheckNotEquals('', ValidarEmail('.comecandocomponto@uol.com.br'));
end;

procedure TTestCaseACBrValidadorEmail.EmailInvalidoComecandoComArroba;
begin
  CheckNotEquals('', ValidarEmail('@example.com'));
end;

procedure TTestCaseACBrValidadorEmail.EmailInvalidoComDoisPontosSeguidos;
begin
  CheckNotEquals('', ValidarEmail('John..Doe@example.com'));
end;

procedure TTestCaseACBrValidadorEmail.EmailInvalidoArrobaComPonto;
begin
  CheckNotEquals('', ValidarEmail('JohnDoe@.example.com.'));
end;

procedure TTestCaseACBrValidadorEmail.EmailInvalidoTerminandoComPonto;
begin
  CheckNotEquals('', ValidarEmail('JohnDoe@example.com.'));
end;

procedure TTestCaseACBrValidadorEmail.EmailInvalidoTerminandoComArroba;
begin
  CheckNotEquals('', ValidarEmail('JohnDoe@'));
end;

procedure TTestCaseACBrValidadorEmail.EmailInvalidoComCarecteresEspeciais;
begin
  CheckNotEquals('', ValidarEmail('cáractersespeciais@empresa.com.br'));
end;

procedure TTestCaseACBrValidadorEmail.ValidarListaEmailsValidos;
begin
  CheckEquals('', ValidarEmail('comercial@djpdv.com.br;financeiro@djpdv.com.br;pessoa@suaempresa.com.br'));
end;

procedure TTestCaseACBrValidadorEmail.ValidarListaEmailsInvalidos;
begin
  CheckNotEquals('', ValidarEmail('comercial@djpdv.com.br;nome com espaco@hotmail.com;pessoa@suaempresa.com.br'));
end;

procedure TTestCaseACBrValidadorEmail.ValidarListaEmailsMisturandoDelimitadores;
begin
  CheckNotEquals('', ValidarEmail('comercial@djpdv.com.br;financeiro@djpdv.com.br,pessoa@suaempresa.com.br'));
end;

procedure TTestCaseACBrValidadorEmail.ValidarEmailsValidos;
begin
  CheckEquals('', ValidarEmail('nome@gmail.com'));
  CheckEquals('', ValidarEmail('nome@hotmail.com'));
  CheckEquals('', ValidarEmail('pessoa@suaempresa.com.br'));
  CheckEquals('', ValidarEmail('pessoa.cadastrada.com.nome.de.email.muito.longo@minhaempresa.com.br'));
end;

{ TTestCaseACBrValidadorPlaca }

procedure TTestCaseACBrValidadorPlaca.SetUp;
begin
  fACBrValidador := TACBrValidador.Create(nil);
  fACBrValidador.TipoDocto := docPlaca;
end;

procedure TTestCaseACBrValidadorPlaca.TearDown;
begin
  inherited;
  FreeAndNil(fACBrValidador);
end;

procedure TTestCaseACBrValidadorPlaca.FormatarPlaca;
begin
   CheckEquals('AAA-9999', ACBrValidador.FormatarPlaca('AAA9999'));
   CheckEquals('AAA-9999', ACBrValidador.FormatarPlaca('aaa9999'));
   CheckEquals('A99-A999', ACBrValidador.FormatarPlaca('a99a999'));
   CheckEquals('A9A-A9A9', ACBrValidador.FormatarPlaca('A9AA9A9'));
   CheckEquals('999-9999', ACBrValidador.FormatarPlaca('9999999'));
   CheckEquals('AAA-AAAA', ACBrValidador.FormatarPlaca('AAAAAAA'));
   CheckEquals('AAA-AAAA', ACBrValidador.FormatarPlaca('aaaaaaa'));
end;

procedure TTestCaseACBrValidadorPlaca.PlacasInvalidas;
begin
   CheckNotEquals('', ValidarPlaca('AA~9999'));
   CheckNotEquals('', ValidarPlaca('^AA9999'));
   CheckNotEquals('', ValidarPlaca('$%$$##$'));
   CheckNotEquals('', ValidarPlaca('A'));
   CheckNotEquals('', ValidarPlaca(''));
   CheckNotEquals('', ValidarPlaca('AAAAAA'));
   CheckNotEquals('', ValidarPlaca('999999'));
end;

procedure TTestCaseACBrValidadorPlaca.PlacasValidas;
begin
   CheckEquals('', ValidarPlaca('AAA9999'));
   CheckEquals('', ValidarPlaca('A99A999'));
   CheckEquals('', ValidarPlaca('AAAAAAA'));
   CheckEquals('', ValidarPlaca('9999999'));
end;

initialization

  RegisterTest(TTestCaseACBrValidadorCPF{$ifndef FPC}.Suite{$endif});
  RegisterTest(TTestCaseACBrValidadorCNPJ{$ifndef FPC}.Suite{$endif});
  RegisterTest(TTestCaseACBrValidadorUF{$ifndef FPC}.Suite{$endif});
  RegisterTest(TTestCaseACBrValidadorIE{$ifndef FPC}.Suite{$endif});
  RegisterTest(TTestCaseACBrValidadorTelefone{$ifndef FPC}.Suite{$endif});
  RegisterTest(TTestCaseACBrValidadorCEP{$ifndef FPC}.Suite{$endif});
  RegisterTest(TTestCaseACBrValidadorEmail{$ifndef FPC}.Suite{$endif});
  RegisterTest(TTestCaseACBrValidadorPlaca{$ifndef FPC}.Suite{$endif});

end.

