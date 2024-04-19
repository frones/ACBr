unit ACBrLibAbecsPinpadTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibAbecsPinpadNome = 'ACBrLibAbecsPinpad';

type

  { TTestACBrAbecsPinpadLib }

  TTestACBrAbecsPinpadLib= class(TTestCase)
  published
    procedure Test_AbecsPinpad_Inicializar_Com_DiretorioInvalido;
    procedure Test_AbecsPinpad_Inicializar;
    procedure Test_AbecsPinpad_Inicializar_Ja_Inicializado;
    procedure Test_AbecsPinpad_Finalizar;
    procedure Test_AbecsPinpad_Finalizar_Ja_Finalizado;
    procedure Test_AbecsPinpad_Nome_Obtendo_LenBuffer;
    procedure Test_AbecsPinpad_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_AbecsPinpad_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_AbecsPinpad_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_AbecsPinpad_Versao;
    procedure Test_AbecsPinpad_ConfigLerValor;
    procedure Test_AbecsPinpad_ConfigGravarValor;


  end;

implementation

uses
  ACBrLibAbecsPinpadStaticImportMT, ACBrLibAbecsPinpadConsts, ACBrLibConsts, ACBrUtil, Dialogs;


procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    //AbecsPinpad_Finalizar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, AbecsPinpad_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini', ''));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Finalizar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
    //AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 0;
  AssertEquals(ErrOk, AbecsPinpad_Nome(Handle,Nil, Bufflen));
  AssertEquals(Length(CLibAbecsPinpadNome), Bufflen);

  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, AbecsPinpad_Inicializar(Handle, '',''));

  Bufflen := Length(CLibAbecsPinpadNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, AbecsPinpad_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibAbecsPinpadNome), Bufflen);
  AssertEquals(CLibAbecsPinpadNome, AStr);

  AssertEquals(ErrOK, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, AbecsPinpad_Inicializar(Handle, '', ''));

  Bufflen := Length(CLibAbecsPinpadNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, AbecsPinpad_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibAbecsPinpadNome), Bufflen);
  AssertEquals(CLibAbecsPinpadNome, AStr);

  AssertEquals(ErrOK, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, AbecsPinpad_Inicializar(Handle,  '', ''));

  Bufflen := 18;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, AbecsPinpad_Nome(Handle,  PChar(AStr), Bufflen));
  AssertEquals(18, Bufflen);
  AssertEquals(copy(CLibAbecsPinpadNome,1,18), AStr);

  AssertEquals(ErrOK, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Versao;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, AbecsPinpad_Inicializar(Handle,  '', ''));

  Bufflen := 0;
  AssertEquals(ErrOk, AbecsPinpad_Versao(Handle,  Nil, Bufflen));
  AssertEquals(Length(CLibAbecsPinpadVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, AbecsPinpad_Versao(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibAbecsPinpadVersao), Bufflen);
  AssertEquals(CLibAbecsPinpadVersao, AStr);

  AssertEquals(ErrOK, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_ConfigLerValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, AbecsPinpad_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, AbecsPinpad_ConfigLerValor(Handle, CSessaoVersao, CLibAbecsPinpadNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibAbecsPinpadVersao, AStr);

  AssertEquals(ErrOK, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_ConfigGravarValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
    // Gravando o valor
  AssertEquals(ErrOK, AbecsPinpad_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Mudar configuração', ErrOk, AbecsPinpad_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, AbecsPinpad_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

  AssertEquals(ErrOK, AbecsPinpad_Finalizar(Handle));
end;

initialization

  RegisterTest(TTestACBrAbecsPinpadLib);
end.

