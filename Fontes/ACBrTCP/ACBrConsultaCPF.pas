{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 07/08/2013: Primeira Versao - Adaptador conforme ACBRConsultaCNPJ
|*    Daniel - schrsistemas@gmail.com
******************************************************************************}

{$I ACBr.inc}

unit ACBrConsultaCPF;

interface

uses
  SysUtils, Classes, ACBrSocket, synacode;

type
  EACBrConsultaCPFException = class ( Exception );

  { TACBrConsultaCPF }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrConsultaCPF = class(TACBrHTTP)
  private
    FDataNascimento: String;
    FDataInscricao: String;
    FNome: String;
    FSituacao: String;
    FCPF: String;
    FDigitoVerificador: String;
    FEmissao: String;
    FCodCtrlControle: String;
    FTokenCaptcha: String;

    function VerificarErros(Str: String): String;
    function LerCampo(Texto: TStringList; NomeCampo: String): String;
  public
    procedure Captcha(Stream: TStream);
    function Consulta(const ACPF, DataNasc, ACaptcha: String;
      ARemoverEspacosDuplos: Boolean = False): Boolean;
    function GetCaptchaURL : String ;
    procedure StringToStream(const AString: string; out AStream: TStream);
  published
    property CPF: String Read FCPF Write FCPF;
    property DataNascimento : String Read FDataNascimento write FDataNascimento;
    property DataInscricao : String Read FDataInscricao write FDataInscricao;
    property Nome: String Read FNome;
    property Situacao: String Read FSituacao;
    property DigitoVerificador: String Read FDigitoVerificador;
    property Emissao: String Read FEmissao;
    property CodCtrlControle: String Read FCodCtrlControle;
  end;

implementation

uses
  ACBrUtil, ACBrValidador, synautil, strutils;

function StrEntreStr(Str, StrInicial, StrFinal: String; ComecarDe: Integer = 1): String;
var
  Ini, Fim: Integer;
begin
  Ini:= PosEx(StrInicial, Str, ComecarDe) + Length(StrInicial);
  if Ini > 0 then
  begin
    Fim:= PosEx(StrFinal, Str, Ini);
    if Fim > 0 then
      Result:= Copy(Str, Ini, Fim - Ini)
    else
      Result:= '';
  end
  else
    Result:= '';
end;

procedure TACBrConsultaCPF.StringToStream(const AString: string; out AStream: TStream);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(AString);
  try
    SS.Position := 0;
    AStream.CopyFrom(SS, SS.Size);
  finally
    SS.Free;
  end;
end;

function TACBrConsultaCPF.GetCaptchaURL : String ;
var
  AURL, Html: String;
begin
  try
    Self.HTTPGet('https://cpf.receita.fazenda.gov.br/situacao/defaultSonoro.asp');
    Html := UTF8ToNativeString(Self.RespHTTP.Text);

    AURL := StrEntreStr(Html, '<img id="imgCaptcha" alt="Código Captcha" src="data:image/png;base64,', '">');
    
	Result := StringReplace(AURL, 'amp;', '', []);
  except
    on E: Exception do
    begin
      raise EACBrConsultaCPFException.Create('Erro na hora de obter a URL do captcha.'+#13#10+E.Message);
    end;
  end;
end;

procedure TACBrConsultaCPF.Captcha(Stream: TStream);
begin
  try
    {HTTPSend.Sock.SSL.SSLType := LT_TLSv1;
    HTTPGet(GetCaptchaURL);

    if HTTPSend.ResultCode = 200 then
    begin
      HTTPSend.Document.Position := 0;
      Stream.CopyFrom(HTTPSend.Document, HTTPSend.Document.Size);
      Stream.Position := 0;}

      StringToStream(DecodeBase64(GetCaptchaURL), Stream);
      Stream.Position:= 0;
    {end;}
  Except
    on E: Exception do begin
      raise EACBrConsultaCPFException.Create('Erro na hora de fazer o download da imagem do captcha.'+#13#10+E.Message);
    end;
  end;
end;

function TACBrConsultaCPF.VerificarErros(Str: String): String;
var
  Res: String;
begin
  Res := '';
  if Res = '' then
    if Pos( ACBrStr('Os caracteres da imagem não foram preenchidos corretamente'), Str) > 0 then
      Res := 'Os caracteres da imagem não foram preenchidos corretamente.';

  if Res = '' then
    if Pos(ACBrStr('O número do CPF não é válido. Verifique se o mesmo foi digitado corretamente.'), Str) > 0 then
      Res := 'O número do CPF não é válido. Verifique se o mesmo foi digitado corretamente.';

  if Res = '' then
    if Pos(ACBrStr('Não existe no Cadastro de Pessoas Jurídicas o número de CPF informado. '+
                   'Verifique se o mesmo foi digitado corretamente.'), Str) > 0 then
      Res := 'Não existe no Cadastro de Pessoas Jurídicas o número de CPF informado. '+
             'Verifique se o mesmo foi digitado corretamente.';

  if Res = '' then
    if Pos(ACBrStr('a. No momento não podemos atender a sua solicitação. Por favor tente mais tarde.'), Str) > 0 then
      Res := 'Erro no site da receita federal. Tente mais tarde.';

  Result := ACBrStr(Res);
end;

function TACBrConsultaCPF.LerCampo(Texto : TStringList ; NomeCampo : String
  ) : String ;
var
  i : integer;
  linha : String;
begin
  NomeCampo := ACBrStr(NomeCampo);
  Result := '';
  for i := 0 to Texto.Count-1 do
  begin
    linha := Texto[i];
    if Pos(NomeCampo, linha) > 0 then
    begin
      Result := Trim(StringReplace(linha, NomeCampo, ' ',[rfReplaceAll]));
      break;
    end;
  end
end;

function TACBrConsultaCPF.Consulta(const ACPF, DataNasc,  ACaptcha: String;
  ARemoverEspacosDuplos: Boolean): Boolean;
var
  Post: TStringStream;
  Erro: String;
  Resposta : TStringList;
begin
  Erro := ValidarCPF( ACPF ) ;
  if Erro <> '' then
     raise EACBrConsultaCPFException.Create(Erro);

  //txtCPF=11122334410&txtToken_captcha_serpro_gov_br=299218104152138191166941752496584741018616278361624164&txtTexto_captcha_serpro_gov_br=ZCI8B9&Enviar=Consultar
  Post:= TStringStream.Create('');
  try
    {Post.WriteString('txtCPF='+OnlyNumber(ACPF)+'&');
    //Post.WriteString('tempTxtNascimento='+dataNasc+'&');
    Post.WriteString('txtToken_captcha_serpro_gov_br='+FTokenCaptcha+'&');
    Post.WriteString('txtTexto_captcha_serpro_gov_br='+Trim(ACaptcha)+'&');
    Post.WriteString('Enviar=Consultar');}

    Post.WriteString('TxtCPF='+ACPF+'&');
    Post.WriteString('txtDataNascimento='+datanasc+'&');
    Post.WriteString('txtToken_captcha_serpro_gov_br='+FTokenCaptcha+'&');
    Post.WriteString('txtTexto_captcha_serpro_gov_br='+Trim(ACaptcha)+'&');
    //Post.WriteString('txtTexto_captcha_serpro_gov_br='+Trim(ACaptcha)+'&');
    Post.WriteString('Enviar=Consultar');

    Post.Position:= 0;

    HttpSend.Clear;
    HttpSend.Document.Position:= 0;
    HttpSend.Document.CopyFrom(Post, Post.Size);
    HTTPSend.MimeType := 'application/x-www-form-urlencoded';
    HTTPPost('https://cpf.receita.fazenda.gov.br/situacao/ConsultaSituacao.asp');

    Erro := VerificarErros(RespHTTP.Text);

    if Erro = '' then
    begin
      Result:= True;
      Resposta := TStringList.Create;
      try
       {$IfDef DELPHI12_UP}  // delphi 2009 em diante
        Resposta.Text := StripHTML(UTF8ToString(RespHTTP.Text));
       {$Else}
        Resposta.Text := StripHTML(UTF8Decode(RespHTTP.Text));
       {$EndIf}
        RemoveEmptyLines( Resposta );

        //DEBUG:
        //Resposta.SaveToFile('C:\temp\cpf.txt');

        FCPF      := LerCampo(Resposta,'N&ordm; do CPF:');
        FNome     := LerCampo(Resposta,'Nome:');
        FDataNascimento := LerCampo(Resposta,'Data Nascimento:');
        FSituacao := LerCampo(Resposta,'Situa&ccedil;&atilde;o Cadastral:');
        FDataInscricao := LerCampo(Resposta,'Data de Inscri&ccedil;&atilde;o no CPF:');
        FEmissao  := LerCampo(Resposta,'Comprovante emitido &agrave;s:');
        FCodCtrlControle   := LerCampo(Resposta,'C&oacute;digo de controle do comprovante:');
        FDigitoVerificador := LerCampo(Resposta,'D&iacute;gito Verificador:');

        if Trim(FNome) = '' then
        begin
          Erro     := LerCampo(Resposta,'Data de nascimento informada');
          if Trim(Erro) <> '' then
            Erro := 'Erro de data';
        end;
      finally
        Resposta.Free;
      end ;

      if Trim(Erro) = 'Erro de data' then
            raise EACBrConsultaCPFException.Create('Data de nascimento divergente da base da Receita Federal.');

      if Trim(FNome) = '' then
        raise EACBrConsultaCPFException.Create('Não foi possível obter os dados.');

      if ARemoverEspacosDuplos then
      begin
        FNome := RemoverEspacosDuplos(FNome);
      end;
    end
    else
    begin
      Result:= False;
      raise EACBrConsultaCPFException.Create(Erro);
    end;
  finally
    Post.Free;
  end;
end;

end.

