unit ACBrLibMailTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestACBrMailLib }

  TTestACBrMailLib = class(TTestCase)
  published
    procedure Test_MAIL_Inicializar_Com_DiretorioInvalido;
    procedure Test_MAIL_Inicializar;
    procedure Test_MAIL_Inicializar_Ja_Inicializado;
    procedure Test_MAIL_Finalizar;
    procedure Test_MAIL_Finalizar_Ja_Finalizado;
    procedure Test_MAIL_Nome_Obtendo_LenBuffer;
    procedure Test_MAIL_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_MAIL_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_MAIL_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_MAIL_Versao;
    procedure Test_MAIL_ConfigLerValor;
    procedure Test_MAIL_ConfigGravarValor;

    procedure Test_MAIL_Clear;

    procedure Test_MAIL_SetSubject;
    procedure Test_MAIL_AddAddress;
    procedure Test_MAIL_AddReplyTo;
    procedure Test_MAIL_AddCC;
    procedure Test_MAIL_AddBCC;
    procedure Test_MAIL_ClearAttachment;
    procedure Test_MAIL_AddAttachment;
    procedure Test_MAIL_AddBody;
    procedure Test_MAIL_AddAltBody;

    procedure Test_MAIL_Send_Com_Thread;
    procedure Test_MAIL_Send_Sem_Thread;

    procedure Test_MAIL_SaveToFile;
  end;

implementation

uses
  ACBrLibMailStaticImport, ACBrLibMailConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrMailLib.Test_MAIL_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, MAIL_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrMailLib.Test_MAIL_Inicializar;
begin
  AssertEquals(ErrOk, MAIL_Inicializar('',''));
end;

procedure TTestACBrMailLib.Test_MAIL_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, MAIL_Inicializar('',''));
end;

procedure TTestACBrMailLib.Test_MAIL_Finalizar;
begin
  AssertEquals(ErrOk, MAIL_Finalizar());
end;

procedure TTestACBrMailLib.Test_MAIL_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, MAIL_Finalizar());
end;

procedure TTestACBrMailLib.Test_MAIL_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, MAIL_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibMailNome), Bufflen);
end;

procedure TTestACBrMailLib.Test_MAIL_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibMailNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibMailNome), Bufflen);
  AssertEquals(CLibMailNome, AStr);
end;

procedure TTestACBrMailLib.Test_MAIL_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibMailNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibMailNome), Bufflen);
  AssertEquals(CLibMailNome, AStr);
end;

procedure TTestACBrMailLib.Test_MAIL_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibMailNome), Bufflen);
  AssertEquals(copy(CLibMailNome,1,4), AStr);
end;

procedure TTestACBrMailLib.Test_MAIL_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, MAIL_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibMailVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibMailVersao), Bufflen);
  AssertEquals(CLibMailVersao, AStr);
end;

procedure TTestACBrMailLib.Test_MAIL_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_ConfigLerValor(CSessaoVersao, CLibMailNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibMailVersao, AStr);
end;

procedure TTestACBrMailLib.Test_MAIL_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, MAIL_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrMailLib.Test_MAIL_Clear;
begin
  // Limpando o e-mail
  AssertEquals('Erro ao limpar o e-mail', ErrOk, MAIL_Clear);
end;

procedure TTestACBrMailLib.Test_MAIL_SetSubject;
begin
  // Definindo o texto referente ao assunto do e-mail
  AssertEquals('Erro ao definir o assunto do e-mail', ErrOk, MAIL_SetSubject('Teste de envio'));
end;

procedure TTestACBrMailLib.Test_MAIL_AddAddress;
begin
  // Adicionando um Endereço de e-mail
  AssertEquals('Erro ao adicionar um endereço de e-mail', ErrOk, MAIL_AddAddress('fulano@provedor.com', 'fulano'));
end;

procedure TTestACBrMailLib.Test_MAIL_AddReplyTo;
begin
  // Adicionando um Endereço de e-mail
  AssertEquals('Erro ao adicionar um endereço de e-mail (ReplyTo)', ErrOk, MAIL_AddReplyTo('beltrano@provedor.com', 'beltrano'));
end;

procedure TTestACBrMailLib.Test_MAIL_AddCC;
begin
  // Adicionando um Endereço de e-mail
  AssertEquals('Erro ao adicionar um endereço de e-mail (CC)', ErrOk, MAIL_AddCC('siclano@provedor.com', 'siclano'));
end;

procedure TTestACBrMailLib.Test_MAIL_AddBCC;
begin
  // Adicionando um Endereço de e-mail
  AssertEquals('Erro ao adicionar um endereço de e-mail (BCC)', ErrOk, MAIL_AddBCC('joao@provedor.com'));
end;

procedure TTestACBrMailLib.Test_MAIL_ClearAttachment;
begin
  // Limpando a lista de anexos
  AssertEquals('Erro ao limpar a lista de anexos', ErrOk, MAIL_ClearAttachment);
end;

procedure TTestACBrMailLib.Test_MAIL_AddAttachment;
begin
  // Adicionando um arquivo como anexo
  AssertEquals('Erro ao adicionar um arquivo como anexo', ErrOk, MAIL_AddAttachment('../../../../../Bem_Vindo_ao_Trunk2.pdf', 'Documentação sobre o novo Trunk', 1));
end;

procedure TTestACBrMailLib.Test_MAIL_AddBody;
begin
  // Adicionando o corpo do e-mail
  AssertEquals('Erro ao adicionar o corpo do e-mail', ErrOk, MAIL_AddBody('Teste Corpo Normal'));
end;

procedure TTestACBrMailLib.Test_MAIL_AddAltBody;
begin
  // Adicionando o corpo alternativo do e-mail
  AssertEquals('Erro ao adicionar o corpo alternativo do e-mail', ErrOk, MAIL_AddAltBody('Teste Corpo Alternativo'));
end;

procedure TTestACBrMailLib.Test_MAIL_Send_Com_Thread;
begin
  // Enviando e-mail com Thread (Desse modo, não é possível detectar erros de conexão)
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_ConfigGravarValor(CSessaoEmail, CChaveEmailSegundoPlano, '1'));
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_ConfigGravar(''));
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_Send);
end;

procedure TTestACBrMailLib.Test_MAIL_Send_Sem_Thread;
begin
  // Enviando e-mail SEM Thread
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_ConfigGravarValor(CSessaoEmail, CChaveEmailSegundoPlano, '0'));
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_ConfigGravar(''));
  AssertEquals('Erro ao enviar o e-mail', ErrExecutandoMetodo, MAIL_Send);
end;

procedure TTestACBrMailLib.Test_MAIL_SaveToFile;
begin
  // Salvando um arquivo
  AssertEquals('Erro ao salvar um arquivo', ErrOk, MAIL_SaveToFile('.\Teste-SaveToFile.eml'));
  AssertTrue( FileExists('.\Teste-SaveToFile.eml') );
end;

initialization
  RegisterTest(TTestACBrMailLib);

end.

