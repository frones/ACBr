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

unit ACBrLibConsultaCPFRespostas;

interface

uses
  SysUtils, Classes, ACBrLibResposta, ACBrLibConfig;

type

  { TLibConsultaCPFConsulta }

  TLibConsultaCPFConsulta = class(TACBrLibRespostaBase)
  private
    FNome: String;
    FSituacao: String;
    FEmissao: String;
    FCodCtrlControle: String;
    FDigitoVerificador: String;
    FDataInscricao: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property Nome: String read FNome write FNome;
    property Situacao: String read FSituacao write FNome;
    property Emissao: String read FEmissao write FEmissao;
    property CodCtrlControle: String read FCodCtrlControle write FCodCtrlControle;
    property DigitoVerificador: String read FDigitoVerificador write FDigitoVerificador;
    property DataInscricao: String read FDataInscricao write FDataInscricao;

  end;

implementation

uses
  ACBrLibConsultaCPFConsts;

{ TLibConsultaCPFConsulta }

constructor TLibConsultaCPFConsulta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;


end.

