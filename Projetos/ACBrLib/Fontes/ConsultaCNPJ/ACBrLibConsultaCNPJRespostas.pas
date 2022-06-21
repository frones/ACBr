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

unit ACBrLibConsultaCNPJRespostas;

interface

uses
  SysUtils, Classes, ACBrLibResposta;

type

  { TLibConsultaCNPJConsulta }

  TLibConsultaCNPJConsulta = class(TACBrLibRespostaBase)
  private
    FEmpresaTipo: String;          
    FRazaoSocial: String;
    FAbertura: TDateTime;
    FFantasia: String;
    FEndereco: String;
    FNumero: String;
    FComplemento: String;
    FBairro: String;
    FCidade: String;
    FUF: String;
    FCEP: String;
    FSituacao: String;
    FCNAE1: String;
    FCNAE2: String;
    FNaturezaJuridica: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property EmpresaTipo: String read FEmpresaTipo write FEmpresaTipo;     
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;     
    property Abertura: TDateTime read FAbertura write FAbertura;
    property Fantasia: String read FFantasia write FFantasia;        
    property Endereco: String read FEndereco write FEndereco;        
    property Numero: String read FNumero write FNumero;          
    property Complemento: String read FComplemento write FComplemento;     
    property Bairro: String read FBairro write FBairro;             
    property Cidade: String read FCidade write FCidade;          
    property UF: String read FUF write FUF;              
    property CEP: String read FCEP write FCEP;
    property Situacao: String read FSituacao write FSituacao;
    property CNAE1: String read FCNAE1 write FCNAE1;
    property CNAE2: String read FCNAE2 write FCNAE2;
    property NaturezaJuridica: String read FNaturezaJuridica write FNaturezaJuridica;
	
  end;

implementation

uses
  ACBrLibConsultaCNPJConsts;

{ TLibConsultaCNPJConsulta }

constructor TLibConsultaCNPJConsulta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;


end.

