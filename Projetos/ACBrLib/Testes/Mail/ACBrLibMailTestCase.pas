{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibMailTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

const
  CLibMailNome = 'ACBrLibMail';

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
  ACBrLibMailStaticImportMT, ACBrLibConsts, ACBrUtil, Dialogs;

procedure TTestACBrMailLib.Test_MAIL_Inicializar_Com_DiretorioInvalido;
var
  Handle: longint;
begin

  try
    MAIL_Finalizar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, MAIL_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMailLib.Test_MAIL_Inicializar;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, MAIL_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_Inicializar_Ja_Inicializado;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_Finalizar;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_Finalizar_Ja_Finalizado;
var
  Handle: longint;
begin

  try
    AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
    AssertEquals(ErrOk, MAIL_Finalizar(Handle));
    //AssertEquals(ErrOk, MAIL_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMailLib.Test_MAIL_Nome_Obtendo_LenBuffer;
var
  Handle: longint;
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, MAIL_Nome(Handle, Nil, Bufflen));
  AssertEquals(Length(CLibMailNome), Bufflen);
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  Bufflen := Length(CLibMailNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibMailNome), Bufflen);
  AssertEquals(CLibMailNome, AStr);
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  Bufflen := Length(CLibMailNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibMailNome), Bufflen);
  AssertEquals(CLibMailNome, AStr);
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibMailNome), Bufflen);
  AssertEquals(copy(CLibMailNome,1,4), AStr);
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_Versao;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, MAIL_Versao(Handle, Nil, Bufflen));
  Assert(Bufflen > 0);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_Versao(Handle, PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert(AStr <> '');
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_ConfigLerValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_ConfigLerValor(Handle, CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_ConfigGravarValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, MAIL_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MAIL_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_Clear;
var
  Handle: longint;
begin
  // Limpando o e-mail
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao limpar o e-mail', ErrOk, MAIL_Clear(Handle));
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_SetSubject;
var
  Handle: longint;
begin
  // Definindo o texto referente ao assunto do e-mail
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao definir o assunto do e-mail', ErrOk, MAIL_SetSubject(Handle, 'Teste de envio'));
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_AddAddress;
var
  Handle: longint;
begin
  // Adicionando um Endereço de e-mail
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao adicionar um endereço de e-mail', ErrOk, MAIL_AddAddress(Handle, 'fulano@provedor.com', 'fulano'));
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_AddReplyTo;
var
  Handle: longint;
begin
  // Adicionando um Endereço de e-mail
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao adicionar um endereço de e-mail (ReplyTo)', ErrOk, MAIL_AddReplyTo(Handle, 'beltrano@provedor.com', 'beltrano'));
  AssertEquals(ErrOk, MAIL_Finalizar(Handle));
end;

procedure TTestACBrMailLib.Test_MAIL_AddCC;
var
  Handle: longint;
begin
  // Adicionando um Endereço de e-mail
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao adicionar um endereço de e-mail (CC)', ErrOk, MAIL_AddCC(Handle, 'siclano@provedor.com', 'siclano'));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

procedure TTestACBrMailLib.Test_MAIL_AddBCC;
var
  Handle: longint;
begin
  // Adicionando um Endereço de e-mail
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao adicionar um endereço de e-mail (BCC)', ErrOk, MAIL_AddBCC(Handle, 'joao@provedor.com'));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

procedure TTestACBrMailLib.Test_MAIL_ClearAttachment;
var
  Handle: longint;
begin
  // Limpando a lista de anexos
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao limpar a lista de anexos', ErrOk, MAIL_ClearAttachment(Handle));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

procedure TTestACBrMailLib.Test_MAIL_AddAttachment;
var
  Handle: longint;
begin
  // Adicionando um arquivo como anexo
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao adicionar um arquivo como anexo', ErrOk, MAIL_AddAttachment(Handle, '../../../../../Bem_Vindo_ao_Trunk2.pdf', 'Documentação sobre o novo Trunk', 1));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

procedure TTestACBrMailLib.Test_MAIL_AddBody;
var
  Handle: longint;
begin
  // Adicionando o corpo do e-mail
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao adicionar o corpo do e-mail', ErrOk, MAIL_AddBody(Handle, 'Teste Corpo Normal'));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

procedure TTestACBrMailLib.Test_MAIL_AddAltBody;
var
  Handle: longint;
begin
  // Adicionando o corpo alternativo do e-mail
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao adicionar o corpo alternativo do e-mail', ErrOk, MAIL_AddAltBody(Handle, 'Teste Corpo Alternativo'));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

procedure TTestACBrMailLib.Test_MAIL_Send_Com_Thread;
var
  Handle: longint;
begin
  // Enviando e-mail com Thread (Desse modo, não é possível detectar erros de conexão)
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_ConfigGravarValor(Handle, CSessaoEmail, CChaveEmailSegundoPlano, '1'));
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_ConfigGravar(Handle, ''));
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_Send(Handle));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

procedure TTestACBrMailLib.Test_MAIL_Send_Sem_Thread;
var
  Handle: longint;
begin
  // Enviando e-mail SEM Thread
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_ConfigGravarValor(Handle, CSessaoEmail, CChaveEmailSegundoPlano, '0'));
  AssertEquals('Erro ao enviar o e-mail', ErrOK, MAIL_ConfigGravar(Handle, ''));
  AssertEquals('Erro ao enviar o e-mail', ErrExecutandoMetodo, MAIL_Send(Handle));
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

procedure TTestACBrMailLib.Test_MAIL_SaveToFile;
var
  Handle: longint;
begin
  // Salvando um arquivo
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
  AssertEquals('Erro ao salvar um arquivo', ErrOk, MAIL_SaveToFile(Handle, '.\Teste-SaveToFile.eml'));
  AssertTrue( FileExists('.\Teste-SaveToFile.eml') );
  AssertEquals(ErrOk, MAIL_Inicializar(Handle, '',''));
end;

initialization
  RegisterTest(TTestACBrMailLib);

end.

