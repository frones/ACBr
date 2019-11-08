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
|* 14/11/2017: Primeira Versao
|*    Filipe de Almeida Sortica
******************************************************************************}

unit ACBrFeriadoWSClass;

interface

uses
  Classes, ACBrBase;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrFeriadoWSClass = class
  protected
    fOwner: TComponent;
    fpURL: String;

    procedure BuscaEfetuada;
    procedure ErrorAbstract;
    procedure TestarToken;
    procedure TestarPathArquivo;
  public
    constructor Create(AOwner: TComponent); virtual;

    procedure Buscar(const AAno: Integer; const AUF: String = '';
      const ACidade: String = ''); virtual;

    property URL: String read fpURL;
  end;

implementation

uses
  SysUtils, ACBrFeriado, ACBrUtil;

{ TACBrFeriadoWSClass }

procedure TACBrFeriadoWSClass.BuscaEfetuada;
begin
  if (Assigned(TACBrFeriado(fOwner).OnBuscaEfetuada)) then
    TACBrFeriado(fOwner).OnBuscaEfetuada(fOwner);
end;

procedure TACBrFeriadoWSClass.Buscar(const AAno: Integer; const AUF,
  ACidade: String);
begin
  ErrorAbstract;
end;

constructor TACBrFeriadoWSClass.Create(AOwner: TComponent);
begin
  inherited Create;
  if not(AOwner is TACBrFeriado) then
    raise EACBrFeriadoException.Create(ACBrStr('Essa classe deve ser instanciada por TACBrFeriado'));

  fOwner := AOwner;
  fpURL  := '';
end;

procedure TACBrFeriadoWSClass.ErrorAbstract;
begin
  raise EACBrFeriadoException.Create(ACBrStr('Nenhum WebService selecionado'));
end;

procedure TACBrFeriadoWSClass.TestarPathArquivo;
begin
  if (TACBrFeriado(fOwner).PathArquivo = EmptyStr) then
    raise EACBrFeriadoException.Create(ACBrStr('Arquivo não informado'));
  if not(FileExists(TACBrFeriado(fOwner).PathArquivo)) then
    raise EACBrFeriadoException.Create(ACBrStr('Arquivo informado não existe'));
end;

procedure TACBrFeriadoWSClass.TestarToken;
begin
  if (TACBrFeriado(fOwner).Token = EmptyStr) then
    raise EACBrFeriadoException.Create(ACBrStr('Token não informado'));
end;

end.
