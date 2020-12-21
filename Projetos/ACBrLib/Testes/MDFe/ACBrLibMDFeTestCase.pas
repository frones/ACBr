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

unit ACBrLibMDFeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibMDFeNome = 'ACBrMDFe32.dll';

type

  { TTestACBrMDFeLib }

  TTestACBrMDFeLib = class(TTestCase)
  published
    procedure Test_MDFe_Inicializar_Com_DiretorioInvalido;
    procedure Test_MDFe_Inicializar;
    procedure Test_MDFe_Inicializar_Ja_Inicializado;
    procedure Test_MDFe_Finalizar;
    procedure Test_MDFe_Finalizar_Ja_Finalizado;
    procedure Test_MDFe_Nome_Obtendo_LenBuffer;
    procedure Test_MDFe_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_MDFe_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_MDFe_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_MDFe_Versao;
    procedure Test_MDFe_ConfigLerValor;
    procedure Test_MDFe_ConfigGravarValor;

    procedure Test_MDFe_StatusServico;

    procedure Test_MDFe_LimparLista;

    procedure Test_MDFe_CarregarXML;
    procedure Test_MDFe_Imprimir;
    procedure Test_MDFe_ImprimirPDF;

    procedure Test_MDFe_CarregarINI;
    procedure Test_MDFe_Assinar;
    procedure Test_MDFe_Validar;
    procedure Test_MDFe_ValidarRegrasdeNegocios;
    procedure Test_MDFe_VerificarAssinatura;

    procedure Test_MDFe_Enviar;
    procedure Test_MDFe_Consultar;
    procedure Test_MDFe_Cancelar;

    procedure Test_MDFe_EnviarEmail;

    procedure Test_MDFe_EnviarEvento;
    procedure Test_MDFe_EnviarEmailEvento;
    procedure Test_MDFe_ImprimirEvento;
    procedure Test_MDFe_ImprimirEventoPDF;

    procedure Test_MDFe_DistribuicaoDFePorUltNSU;
    procedure Test_MDFe_DistribuicaoDFePorNSU;

  end;

implementation

uses
  ACBrLibMDFeStaticImportMT, ACBrLibMDFeConsts, ACBrLibConsts, ACBrLibComum, Dialogs;

procedure TTestACBrMDFeLib.Test_MDFe_Inicializar_Com_DiretorioInvalido;
var
  Handle: longint;
begin
  try
    MDFE_Finalizar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, MDFe_Inicializar(Handle,'C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end
end;

procedure TTestACBrMDFeLib.Test_MDFe_Inicializar;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, MDFE_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Inicializar_Ja_Inicializado;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, MDFE_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Finalizar;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Finalizar_Ja_Finalizado;
var
  Handle: longint;
begin
  try
    AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
    AssertEquals(ErrOk, MDFe_Finalizar(Handle));
    //AssertEquals(ErrOk, MDFe_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end
end;

procedure TTestACBrMDFeLib.Test_MDFe_Nome_Obtendo_LenBuffer;
var
  Handle: longint;
  Bufflen: Integer;
begin

  try
    // Obtendo o Tamanho //
    AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
    Bufflen := 0;
    AssertEquals(ErrOK, MDFe_Nome(Handle, Nil, Bufflen));
    AssertEquals(Length(CLibMDFeNome), Bufflen);
    AssertEquals(ErrOk, MDFe_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMDFeLib.Test_MDFe_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin

  try
     AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
     Bufflen := Length(CLibMDFeNome);
     AStr := Space(Bufflen);
     AssertEquals(ErrOk, MDFe_Nome(Handle, PChar(AStr), Bufflen));
     AssertEquals(Length(CLibMDFeNome), Bufflen);
     AssertEquals(CLibMDFeNome, AStr);
     AssertEquals(ErrOk, MDFe_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMDFeLib.Test_MDFe_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin

  try
     AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
     Bufflen := Length(CLibMDFeNome)*2;
     AStr := Space(Bufflen);
     AssertEquals(ErrOk, MDFe_Nome(Handle, PChar(AStr), Bufflen));
     AStr := copy(AStr, 1, Bufflen);
     AssertEquals(Length(CLibMDFeNome), Bufflen);
     AssertEquals(CLibMDFeNome, AStr);
     AssertEquals(ErrOk, MDFe_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMDFeLib.Test_MDFe_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin

  try
     AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
     Bufflen := 4;
     AStr := Space(Bufflen);
     AssertEquals(ErrOk, MDFe_Nome(Handle, PChar(AStr), Bufflen));
     AssertEquals(Length(CLibMDFeNome), Bufflen);
     AssertEquals(copy(CLibMDFeNome,1,4), AStr);
     AssertEquals(ErrOk, MDFe_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMDFeLib.Test_MDFe_Versao;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, MDFe_Versao(Handle, Nil, Bufflen));
  //AssertEquals(Length(CLibMDFeVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_Versao(Handle, PChar(AStr), Bufflen));
  //AssertEquals(Length(CLibMDFeVersao), Bufflen);
  //AssertEquals(CLibMDFeVersao, AStr);
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_ConfigLerValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrConfigLer, MDFe_ConfigLerValor(Handle, CSessaoVersao, CLibMDFeNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  //AssertEquals(CLibMDFeVersao, AStr);
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_ConfigGravarValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr, Senha: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, MDFe_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

  Senha := StringToB64Crypt('senha', '');

  AssertEquals('Erro ao Configurar Senha', ErrOk,
   MDFe_ConfigGravarValor(Handle, CSessaoEmail, CChaveSenha, PChar(Senha)));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_ConfigLerValor(Handle, CSessaoEmail, CChaveSenha, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Configurar Senha', 'senha', B64CryptToString(AStr, ''));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_StatusServico;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a consulta ao Status de Serviço
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao consultar ao Status de Serviço', ErrExecutandoMetodo, MDFe_StatusServico(Handle, Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_LimparLista;
var
  Handle: longint;
begin
  // Iniciando a Limpeza da Lista de MDF-e
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao limpar a lista de MDF-e', ErrOk, MDFe_LimparLista(Handle));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_CarregarXML;
var
  Handle: longint;
begin
  // Iniciando o Carregamento do XML do MDF-e
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals('Erro ao carregar o XML do MDF-e', ErrOk,
  // MDFe_CarregarXML(Handle, 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\28140417957142000144580170000000031895518397-mdfe.xml'));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Imprimir;
var
  Handle: longint;
begin
  // Iniciando a Impressão do DAMDFe
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Imprimir o DAMDFe', ErrOk, MDFe_Imprimir(Handle,'',1,'',''));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_ImprimirPDF;
var
  Handle: longint;
begin
  // Iniciando a geração do PDF do DAMDFe
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao gerar o PDF do DAMDFe', ErrExecutandoMetodo, MDFe_ImprimirPDF(Handle));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_CarregarINI;
var
  Handle: longint;
begin
  Test_MDFe_LimparLista;

  // Iniciando o Carregamento do INI do MDF-e
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals('Erro ao carregar o INI do MDF-e', ErrExecutandoMetodo,
  // MDFe_CarregarINI(Handle, 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\Modelo-MDFe.ini'));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Assinar;
var
  Handle: longint;
begin
  // Iniciando a Assinatura
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Assinar', ErrOk, MDFe_Assinar(Handle));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Validar;
var
  Handle: longint;
begin
  // Iniciando a Validação
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Validar', ErrOk, MDFe_Validar(Handle));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_ValidarRegrasdeNegocios;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Validação de Regras de Negócios
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Validar regras de negócio', ErrOk,
  //  MDFe_ValidarRegrasdeNegocios(Handle, Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_VerificarAssinatura;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Verificação de Assinatura
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Verificar assinatura', ErrOk,
  //  MDFe_VerificarAssinatura(Handle, Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Enviar;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin

  try
    // Iniciando o Envio do Lote de MDF-e
    AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

    //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

    //Resposta := '';
    //Tamanho := 0;
    //
    //AssertEquals('Erro ao Enviar Lote de MDF-e', ErrOk,
    //MDFe_Enviar(Handle, Handle, True, True, Resposta, Tamanho));
    //
    //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
    AssertEquals(ErrOk, MDFe_Finalizar(Handle));
  except
     on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMDFeLib.Test_MDFe_Consultar;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  Test_MDFe_LimparLista;

  // Iniciando a Consulta
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Consultar o MDF-e', ErrExecutandoMetodo,
  //  MDFe_Consultar(Handle, 'C:\ERP\XML\201808\MDFe\35180804550110000188570010000009491283342822-MDFe.xml', Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));

end;

procedure TTestACBrMDFeLib.Test_MDFe_Cancelar;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  Test_MDFe_LimparLista;

  // Iniciando o Cancelamento
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Cancelar o MDF-e', ErrExecutandoMetodo,
  //  MDFe_Cancelar(Handle, '35180804550110000188570010000009491283342822',
  //               'Desacordo comercial', '04550110000188', 1, Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));

end;

procedure TTestACBrMDFeLib.Test_MDFe_EnviarEmail;
var
  Handle: longint;
  Path, ArqMDFe: String;
begin

  try
     // Iniciando o envio do e-mail
     AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

     //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

     //Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\';
     //ArqMDFe := Path + '28140417957142000144580170000000031895518397-mdfe.xml';
     //
     //AssertEquals('Erro ao enviar o e-mail', ErrOk,
     //MDFe_EnviarEmail(Handle, 'nome@provedor.com', PChar(ArqMDFe), True,
     // 'Teste de envio', '', '', 'Em anexo o MDF-e') );
      AssertEquals(ErrOk, MDFe_Finalizar(Handle));
  except
        on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMDFeLib.Test_MDFe_EnviarEvento;
begin
  //a
end;

procedure TTestACBrMDFeLib.Test_MDFe_EnviarEmailEvento;
var
  Handle: longint;
  Path, ArqMDFe, ArqEvento: String;
begin

  try
      // Iniciando o envio do evento por email
      AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

      //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

      //Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\';
      //ArqMDFe := Path + '28140417957142000144570170000000311556600342-MDFe.xml';
      //ArqEvento := Path + '2814041795714200014457017000000031155660034211011001-procEventoMDFe.xml';
      //
      //AssertEquals('Erro ao enviar email do evento', ErrOk,
      //MDFe_EnviarEmailEvento(Handle, 'nome@provedor.com', PChar(ArqEvento),
      //PChar(ArqMDFe), True, 'Evento', '', '',
      //'Teste de envio de evento por email.'));
      AssertEquals(ErrOk, MDFe_Finalizar(Handle));
  except
      on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrMDFeLib.Test_MDFe_ImprimirEvento;
var
  Handle: longint;
  Path, ArqMDFe, ArqEvento: String;
begin
  // Iniciando a Impressão do Evento
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\';
  //ArqMDFe := Path + '28140417957142000144580170000000031895518397-mdfe.xml';
  //ArqEvento := Path + '2814041795714200014458017000000003189551839711011201-procEventoMDFe.xml';
  //
  //AssertEquals('Erro ao imprimir o evento', ErrOk,
  //  MDFe_ImprimirEvento(Handle, PChar(ArqMDFe), PChar(ArqEvento)));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_ImprimirEventoPDF;
var
  Handle: longint;
  Path, ArqMDFe, ArqEvento: String;
begin
  // Iniciando a geração do PDF do Evento
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\';
  //ArqMDFe := Path + '28140417957142000144580170000000031895518397-mdfe.xml';
  //ArqEvento := Path + '2814041795714200014458017000000003189551839711011201-procEventoMDFe.xml';
  //
  //AssertEquals('Erro ao gerar o PDF do evento', ErrExecutandoMetodo,
  //  MDFe_ImprimirEventoPDF(Handle, PChar(ArqMDFe), PChar(ArqEvento)));
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_DistribuicaoDFePorUltNSU;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Consulta no WebServices DistribuicaoDFe
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Consultar o DistribuicaoDFePorUltNSU', ErrExecutandoMetodo,
  //  MDFe_DistribuicaoDFePorUltNSU(Handle, '04550110000188', '0', Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

procedure TTestACBrMDFeLib.Test_MDFe_DistribuicaoDFePorNSU;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Consulta no WebServices DistribuicaoDFe
  AssertEquals(ErrOk, MDFe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Consultar o DistribuicaoDFePorNSU', ErrExecutandoMetodo,
  //  MDFe_DistribuicaoDFePorNSU(Handle, '04550110000188', '100', Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, MDFe_Finalizar(Handle));
end;

initialization
  RegisterTest(TTestACBrMDFeLib);

end.

