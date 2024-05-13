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

    // Control Commands
    procedure Test_AbecsPinpad_Ativar;
    procedure Test_AbecsPinpad_Desativar;
    procedure Test_AbecsPinpad_OPN;
    procedure Test_AbecsPinpad_CLO;
    procedure Test_AbecsPinpad_CLX_PorMensagem;
    procedure Test_AbecsPinpad_CLX_PorImagem;
    procedure Test_AbecsPinpad_GIX_Todas_Informacoes;
    procedure Test_AbecsPinpad_GIX_Uma_Informacao;
    procedure Test_AbecsPinpad_GIX_MaisDeUma_Informacao;
    procedure Test_AbecsPinpad_GIN;
    procedure Test_AbecsPinpad_PinPadCapabilities;
    procedure Test_AbecsPinpad_DSP;
    procedure Test_AbecsPinpad_ClearDSP;
    procedure Test_AbecsPinpad_DEX;
    procedure Test_AbecsPinpad_ClearDEX;
    procedure Test_AbecsPinpad_GKY;
    procedure Test_AbecsPinpad_RMC;
    procedure Test_AbecsPinpad_GCD;
    procedure Test_AbecsPinpad_CEX_VerifyKey;
    procedure Test_AbecsPinpad_CEX_VerifyMagnetic;
    procedure Test_AbecsPinpad_CEX_VerifyICCInsertion;
    procedure Test_AbecsPinpad_CEX_VerifyICCRemoval;
    procedure Test_AbecsPinpad_CEX_VerifyCTLSPresence;
    procedure Test_AbecsPinpad_CEX_All;
    procedure Test_AbecsPinpad_MNU;
    procedure Test_AbecsPinpad_MNU_Hotkey;
    procedure Test_AbecsPinpad_LoadMedia;
    procedure Test_AbecsPinpad_LMF;
    procedure Test_AbecsPinpad_DSI;
    procedure Test_AbecsPinpad_DMF;

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

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Ativar;
var
  Handle: THandle;
begin
  //Ativando Pinpad
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_Desativar;
var
  Handle: THandle;
begin
  // Desativando Pinpad
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_OPN;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CLO;
var
  handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad Utilizando o Comando CLO', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada CLO'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CLX_PorMensagem;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad Utilizando o Comando CLX', ErrOk, AbecsPinpad_CLX(Handle, 'Comunicação ' +sLineBreak+ ' Finalizada ' +sLineBreak+ ' CLX'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CLX_PorImagem;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad Enviando uma Imagem com o Comando CLX ', ErrOk, AbecsPinpad_CLX(Handle, 'TesteIMG'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_GIX_Todas_Informacoes;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Ler Dados do Pinpad', ErrOk, AbecsPinpad_GIX(Handle, '', PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_GIX_Uma_Informacao;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Ler Dados do Pinpad', ErrOk, AbecsPinpad_GIX(Handle, 'PP_SERNUM', PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_GIX_MaisDeUma_Informacao;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Ler Dados do Pinpad', ErrOk, AbecsPinpad_GIX(Handle, 'PP_SERNUM, PP_PARTNBR', PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_GIN;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Enviar Comando GIN', ErrOk, AbecsPinpad_GIN(Handle, 1, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_PinPadCapabilities;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Ler Capacidades do Pinpad', ErrOk, AbecsPinpad_PinPadCapabilities(Handle, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_DSP;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Enviar Mensagem para o Pinpad', ErrOk, AbecsPinpad_DSP(Handle, 'Teste Linha 1' +#13#10+ 'Teste Linha 2'));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_ClearDSP;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Limpar Display do Pinpad', ErrOk, AbecsPinpad_DSP(Handle, ''));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_DEX;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Enviar Mensagem para o Pinpad', ErrOk, AbecsPinpad_DEX(Handle, 'PROJETO ACBR' +#13#10+ 'ACBrAbecsPinPad' +#13#10+ '----------------' +#13#10+ 'TEF e PIX ?' +#13#10+ 'É no ACBr' +#13#10+ '-+-+-+-+-+-+-+-+ '));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_ClearDEX;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Limpar Display do Pinpad', ErrOk, AbecsPinpad_DEX(Handle, ''));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_GKY;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Usar Comando GKY', ErrOk, AbecsPinpad_GKY(Handle));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_RMC;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Enviar Mensagem para o Pinpad', ErrOk, AbecsPinpad_RMC(Handle, 'OPERATION' +#13#10+ 'FINISHED'));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_GCD;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Capturar dados Pinpad', ErrOk, AbecsPinpad_GCD(Handle, 4, 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CEX_VerifyKey;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Verificar Chave do Pinpad', ErrOk, AbecsPinpad_CEX(Handle, True, False, False, False, False, 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CEX_VerifyMagnetic;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Verificar Leitor Magnetico do Pinpad', ErrOk, AbecsPinpad_CEX(Handle, False, True, False, False, False, 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CEX_VerifyICCInsertion;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Verificar Inserção de cartão no Pinpad', ErrOk, AbecsPinpad_CEX(Handle, False, False, True, False, False, 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CEX_VerifyICCRemoval;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Verificar Remoção de cartão no Pinpad', ErrOk, AbecsPinpad_CEX(Handle, False, False, False, True, False, 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CEX_VerifyCTLSPresence;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Verificar ContactLess no Pinpad', ErrOk, AbecsPinpad_CEX(Handle, False, False, False, False, True, 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_CEX_All;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Realizar o comando CEX', ErrOk, AbecsPinpad_CEX(Handle, True, True, True, True, True, 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_MNU;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Selecionar Opção no Pinpad', ErrOk, AbecsPinpad_MNU(Handle, 'Opcao1, Opcao2, Opcao3', 'Teste Titulo', 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_MNU_Hotkey;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Selecionar Opção no Pinpad', ErrOk, AbecsPinpad_MNU(Handle, '1-Opcao1, 2-Opcao2, 3-Opcao3, 4-Opcao4', 'Teste Titulo Hotkey', 60, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_LoadMedia;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Carregar Midia para o Pinpad', ErrOk, AbecsPinpad_LoadMedia(Handle, 'C:\TesteIMG.jpg', 2, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_LMF;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Listar Media Files', ErrOk, AbecsPinpad_LMF(Handle, PChar(AStr), Bufflen));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_DSI;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Enviar Midia para o Pinpad', ErrOk, AbecsPinpad_DSI(Handle, 'TesteIMG'));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

procedure TTestACBrAbecsPinpadLib.Test_AbecsPinpad_DMF;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, AbecsPinpad_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar Pinpad', ErrOk, AbecsPinpad_Ativar(Handle));
  AssertEquals('Erro ao Abrir Comunicação com Pinpad', ErrOk, AbecsPinpad_OPN(Handle));
  AssertEquals('Erro ao Enviar Midia para o Pinpad', ErrOk, AbecsPinpad_DMF(Handle, 'TesteIMG'));
  AssertEquals('Erro ao Finalizar Comunicação com Pinpad', ErrOk, AbecsPinpad_CLO(Handle, 'Comunicação Finalizada'));
  AssertEquals('Erro ao Desativar Pinpad', ErrOk, AbecsPinpad_Desativar(Handle));
  AssertEquals(ErrOk, AbecsPinpad_Finalizar(Handle));
end;

initialization

  RegisterTest(TTestACBrAbecsPinpadLib);
end.

