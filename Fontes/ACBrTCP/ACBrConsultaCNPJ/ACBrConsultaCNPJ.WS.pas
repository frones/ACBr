{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{$I ACBr.inc}
unit ACBrConsultaCNPJ.WS;

interface
uses
  ACBrJSON, SysUtils, ACBrValidador, httpsend,
  Classes;
type
  EACBrConsultaCNPJWSException = class ( Exception );
  TACBrConsultaCNPJWSResposta = class (TObject)
    NaturezaJuridica     : String ;
    EmpresaTipo          : String;
    Abertura             : TDateTime;
    RazaoSocial          : String;
    Fantasia             : String;
    Porte                : String;
    CNAE1                : String;
    CNAE2                : TStringList;
    Endereco             : String;
    Numero               : String;
    Complemento          : String;
    CEP                  : String;
    Bairro               : String;
    Cidade               : String;
    UF                   : String;
    Situacao             : String;
    SituacaoEspecial     : String;
    CNPJ                 : String;
    DataSituacao         : TDateTime;
    DataSituacaoEspecial : TDateTime;
    EndEletronico        : String;
    Telefone             : String;
    EFR                  : string;
    MotivoSituacaoCad    : string;
    CodigoIBGE           : String;
  end;
  { TACBrConsultaCNPJWS }
  TACBrConsultaCNPJWS = class( TObject )
    FCNPJ : string;
    FUsuario : String;
    FSenha : String;
    FResposta : TACBrConsultaCNPJWSResposta;
  private
    FHTTPSend: THTTPSend;
    public
      constructor create(const ACNPJ : string; AUsuario : string = ''; ASenha: string = '');
      destructor Destroy; override;
      function Executar : boolean; virtual;
      property HTTPSend: THTTPSend read FHTTPSend write FHTTPSend;
  end;
implementation

{ TACBrConsultaCNPJWS }

constructor TACBrConsultaCNPJWS.create(const ACNPJ : string; AUsuario : string = ''; ASenha: string = '');
begin
  FCNPJ     :=  ACNPJ;
  FUsuario  := AUsuario;
  FSenha    := ASenha;
  FResposta := TACBrConsultaCNPJWSResposta.Create;
  FResposta.CNAE2     := TStringList.Create;
end;

destructor TACBrConsultaCNPJWS.Destroy;
begin
  FResposta.CNAE2.Free;
  FResposta.Free;
  inherited;
end;

function TACBrConsultaCNPJWS.Executar: boolean;
var LErro : String;
begin
  Result := False;
  LErro := ValidarCNPJ( FCNPJ ) ;
  if LErro <> '' then
    raise EACBrConsultaCNPJWSException.Create(LErro);
end;

end.
