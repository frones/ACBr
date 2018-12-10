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
|* 24/08/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrCHQImpressoraECF
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQImpressoraECF;

interface
uses ACBrCHQClass,  
     Classes ;

type TACBrCHQImpressoraECF = class( TACBrCHQClass )
  private

  protected
    function GetChequePronto: Boolean; Override ;

  public
    constructor Create(AOwner: TComponent);

    procedure Ativar ; override ;

    procedure ImprimirCheque ; Override ;
    procedure ImprimirLinha( const AString : AnsiString ) ; Override ;

end ;

implementation
Uses ACBrUtil, SysUtils ;

{ TACBrCHQImpressoraECF }

constructor TACBrCHQImpressoraECF.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr    := 'ECF 2 estações' ;
  fpDevice.Porta := '' ;
end;

procedure TACBrCHQImpressoraECF.Ativar;
begin
  if not Assigned( fpECF ) then
     raise Exception.Create(ACBrStr('Para Imp.Cheques "chqImpressoraECF", ACBrCHQ'+
                            ' deve estar ligado a um componente ACBrECF'));

  if not fpECF.Ativo then
     raise Exception.Create(ACBrStr('Para usar Imp.Cheques "chqImpressoraECF", ACBrECF'+
                            ' deve estar Ativo'));

  inherited Ativar ; { Apenas ajusta fpAtivo }
end;

function TACBrCHQImpressoraECF.GetChequePronto: Boolean;
begin
  Result := fpECF.ChequePronto ;
end;

procedure TACBrCHQImpressoraECF.ImprimirCheque;
begin
  fpECF.ImprimeCheque( fpBanco, fpValor, fpFavorecido, fpCidade, fpData,
                       fpObservacao ) ;
end;

procedure TACBrCHQImpressoraECF.ImprimirLinha(const AString: AnsiString);
begin
  { Impressora de Cheques em ECF (2 estações) não permite a impressao de linhas
    no verso... NADA A FAZER }   
end;

end.
 