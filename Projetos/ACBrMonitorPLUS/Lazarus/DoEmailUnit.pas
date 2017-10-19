{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Celso Marigo Junior                             }
{                              Jean Patrick Figueiredo dos Santos              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

unit DoEmailUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CmdUnit;

procedure DoEmail(Cmd: TACBrCmd);
procedure ConfigurarEmailNovo;
procedure ConfigurarDadosEmail(aStr : String );
procedure RecuperarDadosIniciais;
function AdicionaDestino( Endereco, Nome, Tipo : String ) : String;
function ValidarEmail(aEmail: String) : Boolean;

implementation

uses ACBrUtil, mimemess, RegExpr, synachar, IniFiles, DoACBrUnit,
  ACBrMail, typinfo,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

var
  EmailNome, EmailEndereco, EmailHost, EmailUsuario,
  EmailSenha, EmailPorta : String;
  EmailCodificacao : TMimeChar;
  EmailSSL, EmailTLS, FlagEmailNovo : Boolean;

function ValidarEmail(aEmail: String) : Boolean;
var
  vRegex: TRegExpr;
begin
  Result := False;
  vRegex := TRegExpr.Create;
  try
    vRegex.Expression := '^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}' +
                         '\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\' +
                         '.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)$';

    if vRegex.Exec(aEmail) then
       Result := True;

  finally
    vRegex.Free;
  end;
end;

procedure DoEmail(Cmd: TACBrCmd);
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrMail1 {$ELSE}dm.ACBrMail1 {$ENDIF} do
  begin
    if Cmd.Metodo = 'novo' then
     begin
       ConfigurarEmailNovo;
       if ( Cmd.Params(0) <> '' ) then { lê configurações do remetente via ini }
         ConfigurarDadosEmail(Cmd.Params(0))
       else
         { Recupera dados iniciais }
         RecuperarDadosIniciais;

       FlagEmailNovo := True;

       Cmd.Resposta := 'Novo E-mail iniciado!';
     end

    else if not FlagEmailNovo then
     begin
       raise Exception.Create('E-mail não iniciado. Envie um comando EMAIL.Novo')
     end

    else if Cmd.Metodo = 'adicionapara' then {adiciona remetente}
     begin
       if not ValidarEmail(Cmd.Params(0)) then
          raise Exception.Create(Cmd.Params(0)+' não é um E-mail válido.')
       else
          Cmd.Resposta := AdicionaDestino( Cmd.Params(0), Cmd.Params(1), 'Para' );

     end

    else if Cmd.Metodo = 'adicionarespondera' then {adiciona e-mail para resposta}
     begin
       if not ValidarEmail(Cmd.Params(0)) then
          raise Exception.Create(Cmd.Params(0)+' não é um E-mail válido.')
       else
          Cmd.Resposta := AdicionaDestino( Cmd.Params(0), Cmd.Params(1), 'ReplyTo' );

     end

    else if Cmd.Metodo = 'adicionacc' then {adiciona e-mail "Com copia"}
     begin
       if not ValidarEmail(Cmd.Params(0)) then
          raise Exception.Create(Cmd.Params(0)+' não é um E-mail válido.')
       else
          Cmd.Resposta := AdicionaDestino( Cmd.Params(0), Cmd.Params(1), 'CC' );

     end

    else if Cmd.Metodo = 'adicionabcc' then {adiciona e-mail "Com Copia Oculta"}
     begin
       if not ValidarEmail(Cmd.Params(0)) then
          raise Exception.Create(Cmd.Params(0)+' não é um E-mail válido.')
       else
          Cmd.Resposta := AdicionaDestino( Cmd.Params(0), '', 'BCC' );

     end

    else if Cmd.Metodo = 'assunto' then {assunto do e-mail}
       Subject := Cmd.Params(0)

    else if Cmd.Metodo = 'confirmarleitura' then {seta confirmação de leitura (Falso)}
       ReadingConfirmation := StrToBoolDef(Trim(Cmd.Params(0)),False)

    else if Cmd.Metodo = 'usarhtml' then {seta formato de entrega (False)}
       IsHTML := StrToBoolDef(Trim(Cmd.Params(0)),False)

    else if Cmd.Metodo = 'tentativasenvio' then {seta numero de tentativas de envio (1)}
       Attempts := StrToInt( Cmd.Params(0) )

    else if Cmd.Metodo = 'setprioridade' then  {seta prioridade (Normal)}
     begin
       case Cmd.Params(0) of
         'naodefinida' : Priority := MP_unknown;
         'alta'        : Priority := MP_high;
         'baixa'       : Priority := MP_low;
       else
         Priority := MP_normal;
       end;
     end

    else if Cmd.Metodo = 'setcodificacao' then  {seta codificacao (DefaultCharset)}
     begin
       DefaultCharset := GetCPFromID( Cmd.Params(0) );
       Cmd.Resposta := 'Codificação setada para ' + Cmd.Params(0);
     end

    else if Cmd.Metodo = 'textomensagem' then {adiciona o texto ao corpo do e-mail}
       Body.Add( Cmd.Params(0) )

    else if Cmd.Metodo = 'textoalternativo' then {adiciona o texto alternativo (texto puro) do e-mail}
       AltBody.Add( Cmd.Params(0) )

    else if Cmd.Metodo = 'adicionaanexo' then {adiciona anexo}
     begin
       AddAttachment(Cmd.Params(0), Cmd.Params(1));
       Cmd.Resposta := 'Anexo incluído com sucesso!';
     end

    else if Cmd.Metodo = 'enviar' then {envia mensagem}
     begin
       try
         FlagEmailNovo := False;

         Send;

         Cmd.Resposta := 'E-mail enviado com sucesso!';
       except
         on E: Exception do
         begin
           { Recupera dados iniciais }
           RecuperarDadosIniciais;
           raise Exception.Create(e.Message);
         end;
       end;

       { Recupera dados iniciais }
       RecuperarDadosIniciais;
     end

    else
       raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;

  end;
end;

function AdicionaDestino(Endereco, Nome, Tipo: String): String;
begin
  Result := '';

  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrMail1 {$ELSE}dm.ACBrMail1 {$ENDIF} do
  begin
    if ( Tipo = 'Para' ) then
       AddAddress( Endereco, Nome )
    else if ( Tipo = 'CC' ) then
       AddCC( Endereco, Nome )
    else if ( Tipo = 'BCC' ) then
       AddBCC( Endereco )
    else
       AddReplyTo( Endereco, Nome );

  end;

  Result := 'E-mail ' + Endereco + ' adicionado a lista "' + Tipo + '".';
end;

procedure ConfigurarEmailNovo;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrMail1 {$ELSE}dm.ACBrMail1 {$ENDIF} do
  begin
    { Salva dados iniciais }
    EmailEndereco    := From;
    EmailNome        := FromName;
    EmailHost        := Host;
    EmailUsuario     := Username;
    EmailSenha       := Password;
    EmailPorta       := Port;
    EmailSSL         := SetSSL;
    EmailTLS         := SetTLS;
    EmailCodificacao := DefaultCharset;

    { Limpa todos os dados do componente ACBrMail }
    Clear;
  end;
end;

procedure ConfigurarDadosEmail(aStr: String);
var
  IniDados : TMemIniFile;
  sCharset : String;
begin
  IniDados   := LerConverterIni(aStr);
  try
    if IniDados.SectionExists('EMAIL') then
    begin
      with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrMail1 {$ELSE}dm.ACBrMail1 {$ENDIF} do
      begin
        From           := IniDados.ReadString('EMAIL', 'Endereco', EmailEndereco);
        FromName       := IniDados.ReadString('EMAIL', 'NomeExibicao', EmailNome);
        Host           := IniDados.ReadString('EMAIL', 'Email', EmailHost);
        Username       := IniDados.ReadString('EMAIL', 'Usuario', EmailUsuario);
        Password       := IniDados.ReadString('EMAIL', 'Senha', EmailSenha);
        Port           := IniDados.ReadString('EMAIL', 'Porta', EmailPorta);
        SetSSL         := IniDados.ReadBool(  'EMAIL', 'ExigeSSL', EmailSSL);
        SetTLS         := IniDados.ReadBool(  'EMAIL', 'ExigeTLS', EmailTLS);
        sCharset       := IniDados.ReadString('EMAIL', 'Codificacao', '');

        if ( sCharset <> '' ) then
           DefaultCharset := TMailCharset(GetEnumValue(TypeInfo(TMailCharset),
             sCharset))

      end;
    end;
  finally
    IniDados.Free;
  end;
end;

procedure RecuperarDadosIniciais;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrMail1 {$ELSE}dm.ACBrMail1 {$ENDIF} do
  begin
    if ( (Trim(From) = '') or not(ValidarEmail(From)) ) then
       raise Exception.Create('E-mail do remetente não informado ou inválido!')
    else if Trim(Host) = '' then
       raise Exception.Create('Host SMTP não informado!')
    else if (Trim(Port) = '') or (Port = '0') then
       raise Exception.Create('Porta SMTP não informada ou inválida!');

    From           := EmailEndereco;
    FromName       := EmailNome;
    Host           := EmailHost;
    Username       := EmailUsuario;
    Password       := EmailSenha;
    Port           := EmailPorta;
    SetSSL         := EmailSSL;
    SetTLS         := EmailTLS;
    DefaultCharset := EmailCodificacao;
  end;
end;

end.

