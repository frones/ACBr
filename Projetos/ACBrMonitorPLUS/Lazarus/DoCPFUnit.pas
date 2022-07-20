{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: José M S Junior                                  }
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

unit DoCPFUnit;

interface

uses
  Classes, SysUtils, ACBrConsultaCPF, ACBrMonitorConsts, ACBrMonitorConfig,
  ACBrLibResposta,  ACBrLibConsultaCPFRespostas,
  CmdUnit, ACBrUtil.FilesIO, ACBrUtil.Strings;

Const
  CCAPTCHA_CPF = 'CaptchaCPF';

Type

{ TACBrObjetoConsultaCPF }

TACBrObjetoConsultaCPF = class(TACBrObjetoDFe)
private
  fACBrConsultaCPF: TACBrConsultaCPF;
public
  constructor Create(AConfig: TMonitorConfig; ACBrConsultaCPF: TACBrConsultaCPF); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaConsulta;
  function EncodeCaptchaBase64(AStream: TMemoryStream):AnsiString;
  procedure DecodeCaptchaBase64(const ABase64Captcha: Ansistring; AStream: TMemoryStream);

  property ACBrConsultaCPF: TACBrConsultaCPF read fACBrConsultaCPF;
end;

{ TMetodoConsultarCaptcha}
TMetodoConsultarCaptcha = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultar}
TMetodoConsultar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

uses
  synautil, Graphics, synacode, pcnAuxiliar;

{ TMetodoConsultar }

{ Params: 0 - String: CPF para Consulta
          1 - String: Data de Nascimento
          2 - String: Captcha para Consulta}
procedure TMetodoConsultar.Executar;
var
  ACPF: String;
  ADataNascimento: String;
  ACaptcha: String;
begin
  ACPF := trim(fpCmd.Params(0));
  ADataNascimento := trim(fpCmd.Params(1));
  ACaptcha := trim(fpCmd.Params(2));

  if (ACPF = '') then
    raise Exception.Create('CPF não informado.')
  else
  begin
    if not( ValidarCPF(ACPF) ) then
      raise Exception.Create('CPF ' + ACPF + ' inválido.');
  end;

  if (ADataNascimento = '') then
    raise Exception.Create('Data Nascimento não informado.');

  if (ACaptcha = '') then
    raise Exception.Create('Captcha não informado.');

  with TACBrObjetoConsultaCPF(fpObjetoDono) do
  begin
    ACBrConsultaCPF.Consulta(ACPF, StringReplace(ADataNascimento, '/', '', [rfReplaceAll]), ACaptcha);
    RespostaConsulta;
  end;

end;

{ TMetodoConsultarCaptcha }

{ Params: 0 - String: Path para gerar png Captcha}
procedure TMetodoConsultarCaptcha.Executar;
var
  AStream: TMemoryStream;
  APath: String;
begin
  APath := trim(fpCmd.Params(0));

  if (APath <> '') and not( DirectoryExists( PathWithDelim( APath ) )) then
      raise Exception.Create('Diretorio: ' + PathWithDelim( APath ) +' não encontrado.');

  AStream:= TMemoryStream.Create;
  try
    with TACBrObjetoConsultaCPF(fpObjetoDono) do
    begin
      ACBrConsultaCPF.Captcha(AStream);
      if (APath <> '') then
      begin
        try
          AStream.SaveToFile( PathWithDelim( APath ) + CCAPTCHA_CPF + '.png' );
          fpCmd.Resposta := PathWithDelim( APath ) + CCAPTCHA_CPF + '.png';
        except
          raise Exception.Create(ACBrStr('Falha ao gravar arquivo ' + CCAPTCHA_CPF + '.png'));
        end;
      end
      else
      begin
        fpCmd.Resposta :=  EncodeCaptchaBase64(AStream);

      end;

    end;
  finally
    AStream.Free;

  end;
end;


{ TACBrObjetoConsultaCPF }

constructor TACBrObjetoConsultaCPF.Create(AConfig: TMonitorConfig;
  ACBrConsultaCPF: TACBrConsultaCPF);
begin
  inherited Create(AConfig);

  fACBrConsultaCPF := ACBrConsultaCPF;

  ListaDeMetodos.Add(CMetodoConsultarCaptcha);
  ListaDeMetodos.Add(CMetodoConsultar);

end;

procedure TACBrObjetoConsultaCPF.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoConsultarCaptcha;
    1  : AMetodoClass := TMetodoConsultar;

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

procedure TACBrObjetoConsultaCPF.RespostaConsulta;
var
  Resp: TLibConsultaCPFConsulta;
begin
  Resp := TLibConsultaCPFConsulta.Create(TpResp, codUTF8);
  try
    with fACBrConsultaCPF do
    begin
      Resp.Nome:= Nome;
      Resp.Situacao:= Situacao;
      Resp.Emissao:= Emissao;
      Resp.CodCtrlControle:= CodCtrlControle;
      Resp.DigitoVerificador:= DigitoVerificador;
      Resp.DataInscricao:= DataInscricao;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;

end;

function TACBrObjetoConsultaCPF.EncodeCaptchaBase64(AStream: TMemoryStream): AnsiString;
var
  ADataPng: AnsiString;
begin
  Result:= '';
  try
    AStream.Position:= 0;
    ADataPng := ReadStrFromStream(AStream, AStream.Size);
    Result := EncodeBase64(ADataPng);

  Except
    raise Exception.Create('Falha ao Converter imagem para base64 ');
  end;
end;

procedure TACBrObjetoConsultaCPF.DecodeCaptchaBase64(const ABase64Captcha: Ansistring; AStream: TMemoryStream);
var
  ADataPng: AnsiString;
begin
  try
     ADataPng := DecodeBase64(ABase64Captcha);
     AStream.Position:= 0;
     WriteStrToStream(AStream, ADataPng);

  Except
    raise Exception.Create('Falha ao Decodificar base64 da imagem ');
  end;

end;

end.

