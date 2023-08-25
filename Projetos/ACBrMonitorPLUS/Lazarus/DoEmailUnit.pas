{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}
{$I ACBr.inc}

unit DoEmailUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrMail, ACBrMonitorConsts, ACBrMonitorConfig, blcksock;

type

{ TACBrObjetoEMail }

TACBrObjetoEMail = class(TACBrObjetoDFe)
private
  fACBrEMail: TACBrMail;

public
  constructor Create(AConfig: TMonitorConfig; ACBrEMail: TACBrMail); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure ConfigurarEmailNovo;
  procedure ConfigurarDadosEmail(aStr : String );
  procedure RecuperarDadosIniciais;
  function AdicionaDestino( Endereco, Nome, Tipo : String ) : String;
  function ValidarEmail(aEmail: String) : Boolean;
  procedure ChecarEmailNovo;

  property ACBrEMail: TACBrMail read fACBrEMail;

end;

{ TMetodoNovo}
TMetodoNovo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionaPara}
TMetodoAdicionaPara = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionaResponderA}
TMetodoAdicionaResponderA = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionaCC}
TMetodoAdicionaCC = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionaBCC}
TMetodoAdicionaBCC = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssunto}
TMetodoAssunto = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConfirmarLeitura}
TMetodoConfirmarLeitura = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoUsarHTML}
TMetodoUsarHTML = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTentativasEnvio}
TMetodoTentativasEnvio = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetPrioridade}
TMetodoSetPrioridade = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetCodificacao}
TMetodoSetCodificacao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTextoMensagem}
TMetodoTextoMensagem = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTextoAlternativo}
TMetodoTextoAlternativo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionaAnexo}
TMetodoAdicionaAnexo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviar}
TMetodoEnviar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

uses
  mimemess, RegExpr, synachar, IniFiles, UtilUnit;

var
  EmailNome, EmailEndereco, EmailHost, EmailUsuario,
  EmailSenha, EmailPorta : String;
  EmailCodificacao : TMimeChar;
  EmailSSL, EmailTLS, FlagEmailNovo : Boolean;
  EmailSSLType : integer;

{ TACBrObjetoEMail }

constructor TACBrObjetoEMail.Create(AConfig: TMonitorConfig; ACBrEMail: TACBrMail);
begin
  inherited Create(AConfig);

  fACBrEMail := ACBrEMail;

  ListaDeMetodos.Add(CMetodoNovo);
  ListaDeMetodos.Add(CMetodoAdicionaPara);
  ListaDeMetodos.Add(CMetodoAdicionaResponderA);
  ListaDeMetodos.Add(CMetodoAdicionaCC);
  ListaDeMetodos.Add(CMetodoAdicionaBCC);
  ListaDeMetodos.Add(CMetodoAssunto);
  ListaDeMetodos.Add(CMetodoConfirmarLeitura);
  ListaDeMetodos.Add(CMetodoUsarHTML);
  ListaDeMetodos.Add(CMetodoTentativasEnvio);
  ListaDeMetodos.Add(CMetodoSetPrioridade);
  ListaDeMetodos.Add(CMetodoSetCodificacao);
  ListaDeMetodos.Add(CMetodoTextoMensagem);
  ListaDeMetodos.Add(CMetodoTextoAlternativo);
  ListaDeMetodos.Add(CMetodoAdicionaAnexo);
  ListaDeMetodos.Add(CMetodoEnviar);

end;

procedure TACBrObjetoEMail.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoNovo;
    1  : AMetodoClass := TMetodoAdicionaPara;
    2  : AMetodoClass := TMetodoAdicionaResponderA;
    3  : AMetodoClass := TMetodoAdicionaCC;
    4  : AMetodoClass := TMetodoAdicionaBCC;
    5  : AMetodoClass := TMetodoAssunto;
    6  : AMetodoClass := TMetodoConfirmarLeitura;
    7  : AMetodoClass := TMetodoUsarHTML;
    8  : AMetodoClass := TMetodoTentativasEnvio;
    9  : AMetodoClass := TMetodoSetPrioridade;
    10 : AMetodoClass := TMetodoSetCodificacao;
    11 : AMetodoClass := TMetodoTextoMensagem;
    12 : AMetodoClass := TMetodoTextoAlternativo;
    13 : AMetodoClass := TMetodoAdicionaAnexo;
    14 : AMetodoClass := TMetodoEnviar;
  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      Ametodo.Free;
    end;
  end;
end;

procedure TACBrObjetoEMail.ConfigurarEmailNovo;
begin
  with fACBrEMail do
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
    EmailSSLType     := Integer(SSLType);

    { Limpa todos os dados do componente ACBrMail }
    Clear;
  end;
end;

procedure TACBrObjetoEMail.ConfigurarDadosEmail(aStr: String);
var
  IniDados: TMemIniFile;
  sCharset: String;
begin
  IniDados := LerConverterIni(aStr);
  try
    if IniDados.SectionExists('EMAIL') then
    begin
      with fACBrEMail do
      begin
        From     := IniDados.ReadString('EMAIL', 'Endereco', EmailEndereco);
        FromName := IniDados.ReadString('EMAIL', 'NomeExibicao', EmailNome);
        Host     := IniDados.ReadString('EMAIL', 'Email', EmailHost);
        Username := IniDados.ReadString('EMAIL', 'Usuario', EmailUsuario);
        Password := IniDados.ReadString('EMAIL', 'Senha', EmailSenha);
        Port     := IniDados.ReadString('EMAIL', 'Porta', EmailPorta);
        SetSSL   := IniDados.ReadBool(  'EMAIL', 'ExigeSSL', EmailSSL);
        SetTLS   := IniDados.ReadBool(  'EMAIL', 'ExigeTLS', EmailTLS);
        sCharset := IniDados.ReadString('EMAIL', 'Codificacao', '');
        SSLType  := TSSLType( IniDados.ReadInteger ('EMAIL','SSLType',EmailSSLType));
        if ( sCharset <> '' ) then
          DefaultCharset := TMailCharset(GetEnumValue(TypeInfo(TMailCharset),sCharset))
      end;
    end;
  finally
    IniDados.Free;
  end;
end;

procedure TACBrObjetoEMail.RecuperarDadosIniciais;
begin
  with fACBrEMail do
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
    SSLType        := TSSLType( EmailSSLType);
  end;
end;

function TACBrObjetoEMail.AdicionaDestino(Endereco, Nome, Tipo: String): String;
begin
  Result := '';

  with fACBrEMail do
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

function TACBrObjetoEMail.ValidarEmail(aEmail: String): Boolean;
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

procedure TACBrObjetoEMail.ChecarEmailNovo;
begin
  if not FlagEmailNovo then
    raise Exception.Create('E-mail não iniciado. Envie um comando EMAIL.Novo');
end;

{ TMetodoNovo }

{ Params: 0 - Arquivo INI contendo a configuração
}
procedure TMetodoNovo.Executar;
var
  AArq: String;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ConfigurarEmailNovo;
    if ( AArq <> '' ) then { lê configurações do remetente via ini }
      ConfigurarDadosEmail(AArq)
    else
      { Recupera dados iniciais }
      RecuperarDadosIniciais;

    FlagEmailNovo := True;

    fpCmd.Resposta := 'Novo E-mail iniciado!';
  end;
end;

{ TMetodoAdicionaPara }

{ Params: 0 - string contendo o e-mail
          1 - string contendo o nome
}
procedure TMetodoAdicionaPara.Executar;
var
  Aemail, ANome: String;
begin
  Aemail := fpCmd.Params(0);
  ANome  := fpCmd.Params(1);

  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    if not ValidarEmail(Aemail) then
       raise Exception.Create(Aemail+' não é um E-mail válido.')
    else
       fpCmd.Resposta := AdicionaDestino( Aemail, ANome, 'Para' );
  end;
end;

{ TMetodoAdicionaResponderA }

{ Params: 0 - string contendo o e-mail
          1 - string contendo o nome
}
procedure TMetodoAdicionaResponderA.Executar;
var
  Aemail, ANome: String;
begin
  Aemail := fpCmd.Params(0);
  ANome  := fpCmd.Params(1);

  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    if not ValidarEmail(Aemail) then
       raise Exception.Create(Aemail+' não é um E-mail válido.')
    else
       fpCmd.Resposta := AdicionaDestino( Aemail, ANome, 'ReplyTo' );
  end;
end;

{ TMetodoAdicionaCC }

{ Params: 0 - string contendo o e-mail
          1 - string contendo o nome
}
procedure TMetodoAdicionaCC.Executar;
var
  Aemail, ANome: String;
begin
  Aemail := fpCmd.Params(0);
  ANome  := fpCmd.Params(1);

  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    if not ValidarEmail(Aemail) then
       raise Exception.Create(Aemail+' não é um E-mail válido.')
    else
       fpCmd.Resposta := AdicionaDestino( Aemail, ANome, 'CC' );
  end;
end;

{ TMetodoAdicionaBCC }

{ Params: 0 - string contendo o e-mail
}
procedure TMetodoAdicionaBCC.Executar;
var
  Aemail: String;
begin
  Aemail := fpCmd.Params(0);

  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    if not ValidarEmail(Aemail) then
       raise Exception.Create(Aemail+' não é um E-mail válido.')
    else
       fpCmd.Resposta := AdicionaDestino( Aemail, '', 'BCC' );
  end;
end;

{ TMetodoAssunto }

{ Params: 0 - string contendo o assunto
}
procedure TMetodoAssunto.Executar;
begin
  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    fACBrEMail.Subject := fpCmd.Params(0);
  end;
end;

{ TMetodoConfirmarLeitura }

{ Params: 0 - inteiro
               0 = False 1 = True
}
procedure TMetodoConfirmarLeitura.Executar;
begin
  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    fACBrEMail.ReadingConfirmation := StrToBoolDef(Trim(fpCmd.Params(0)), False);
  end;
end;

{ TMetodoUsarHTML }

{ Params: 0 - inteiro
               0 = False 1 = True
}
procedure TMetodoUsarHTML.Executar;
begin
  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    fACBrEMail.IsHTML := StrToBoolDef(Trim(fpCmd.Params(0)), False);
  end;
end;

{ TMetodoTentativasEnvio }

{ Params: 0 - inteiro - Numero de tentativas
}
procedure TMetodoTentativasEnvio.Executar;
begin
  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    fACBrEMail.Attempts := StrToInt( fpCmd.Params(0) );
  end;
end;

{ TMetodoSetPrioridade }

{ Params: 0 - String - tipo de prioridade
}
procedure TMetodoSetPrioridade.Executar;
var
  APrior: String;
begin
  APrior := trim(fpCmd.Params(0));

  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    if APrior = 'naodefinida' then
      fACBrEMail.Priority := MP_unknown
    else
      if APrior = 'alta' then
        fACBrEMail.Priority := MP_high
      else
        if APrior = 'baixa' then
          fACBrEMail.Priority := MP_low
        else
          fACBrEMail.Priority := MP_normal;
(*
    case fpCmd.Params(0) of
      'naodefinida' : fACBrEMail.Priority := MP_unknown;
      'alta'        : fACBrEMail.Priority := MP_high;
      'baixa'       : fACBrEMail.Priority := MP_low;
    else
      fACBrEMail.Priority := MP_normal;
    end;
*)
  end;
end;

{ TMetodoSetCodificacao }

{ Params: 0 - String - Codificacao
}
procedure TMetodoSetCodificacao.Executar;
var
  ACod: String;
begin
  ACod := fpCmd.Params(0);

  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    fACBrEMail.DefaultCharset := GetCPFromID( ACod );
    fpCmd.Resposta := 'Codificação setada para ' + ACod;
  end;
end;

{ TMetodoTextoMensagem }

{ Params: 0 - String - contendo a mensagem
}
procedure TMetodoTextoMensagem.Executar;
begin
  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    fACBrEMail.Body.Add( fpCmd.Params(0) );
  end;
end;

{ TMetodoTextoAlternativo }

{ Params: 0 - String - contendo o texto alternativo
}
procedure TMetodoTextoAlternativo.Executar;
begin
  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    fACBrEMail.AltBody.Add( fpCmd.Params(0) );
  end;
end;

{ TMetodoAdicionaAnexo }

{ Params: 0 - string contendo o path e nome do arquivo
          1 - string contendo a descrição
          2 - Integer definindo anexo como inline ou attachement
}
procedure TMetodoAdicionaAnexo.Executar;
var
  AArq, ADesc: String;
  aAttach: Integer;
begin
  AArq := fpCmd.Params(0);
  ADesc  := fpCmd.Params(1);
  aAttach := StrToIntDef( fpCmd.Params(2), 0 );  //Default = attachement

  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    fACBrEMail.AddAttachment(AArq, ADesc, TMailAttachmentDisposition( aAttach ) );
    fpCmd.Resposta := 'Anexo incluído com sucesso!';
  end;
end;

{ TMetodoEnviar }

procedure TMetodoEnviar.Executar;
begin
  with TACBrObjetoEMail(fpObjetoDono) do
  begin
    ChecarEmailNovo;

    try
      FlagEmailNovo := False;

      fACBrEMail.Send;

      if not(fACBrEMail.UseThread) then
        fpCmd.Resposta := 'E-mail enviado com sucesso!'
      else
        fpCmd.Resposta := 'Enviando e-mail em segundo plano...';

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
  end;
end;

end.
