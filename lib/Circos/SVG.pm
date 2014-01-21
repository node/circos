package Circos::SVG;

=pod

=head1 NAME
    
Circos::SVG - utility routines for SVG in Circos

=head1 SYNOPSIS

This module is not meant to be used directly.

=head1 DESCRIPTION

Circos is an application for the generation of publication-quality,
circularly composited renditions of genomic data and related
annotations.

Circos is particularly suited for visualizing alignments, conservation
and intra and inter-chromosomal relationships. However, Circos can be
used to plot any kind of 2D data in a circular layout - its use is not
limited to genomics. Circos' use of lines to relate position pairs
(ribbons add a thickness parameter to each end) is effective to
display relationships between objects or positions on one or more
scales.

All documentation is in the form of tutorials at L<http://www.circos.ca>.

=cut

# -------------------------------------------------------------------

use strict;
use warnings;

use base 'Exporter';
our @EXPORT = qw(
									style_string
									style
							 );

use Carp qw( carp confess croak );
use FindBin;
use GD::Image;
use Params::Validate qw(:all);

use lib "$FindBin::RealBin";
use lib "$FindBin::RealBin/../lib";
use lib "$FindBin::RealBin/lib";

use Circos::Configuration; 
use Circos::Colors;
use Circos::Constants;
use Circos::Debug;
use Circos::Error;
use Circos::Utils;
use Circos::Font;
use Circos::Geometry;
use Circos::Text;
use Circos::Unit;
use Circos::Image qw(!draw_line);

use Memoize;

our $default_color      = "black";
our $default_font_name  = "Arial";
our $default_font       = "default";
our $default_font_color = "black";

for my $f ( qw ( ) ) {
	memoize($f);
}

sub draw_polygon {
	my %params;
	if ( fetch_conf("debug_validate") ) {
		%params = validate(@_,{
													 polygon          => 1,
													 thickness        => 0,
													 color            => 0,
													 fill_color       => 0,
													 linecap          => 0,
													});
	} else {
		%params = @_;
		$params{"stroke-linecap"} ||= "round";
	}
	my $pts = join( $SPACE, map { join( $COMMA, @$_ ) } $params{polygon}->vertices );
	my $style = style(thickness => $params{thickness},
										color     => $params{color},
										linecap   => $params{linecap},
										fill_color=> $params{fill_color});
	my $svg = sprintf(q{<polygon points="%s" style="%s"/>},$pts,$style);
	Circos::printsvg($svg);
}

# Edited on the island of Capri :)
sub draw_circle {
	my %params;
	if ( fetch_conf("debug_validate") ) {
		%params = validate(@_,{
													 point             => 1,
													 radius            => 1,
													 color             => 0,
													 stroke_color      => 0,
													 stroke_thickness  => 0,
													});
	} else {
		%params = @_;
	}
	my $style = style(thickness => $params{stroke_thickness},
										color     => $params{stroke_color},
										fill_color=> $params{color});
	my $fmt = "'%.1f'";
	my $svg = sprintf(qq{<circle cx=$fmt cy=$fmt r=$fmt style="%s"/>},
										@{$params{point}},$params{radius},$style);
	Circos::printsvg($svg);
}

################################################################
# Draw a line

sub draw_line {
	my %params;
	if ( fetch_conf("debug_validate") ) {
		%params = validate(@_,{
													 points           => { type    => ARRAYREF },
													 color            => { default => fetch_conf("default_color") || $default_color  },
													 thickness        => { default => 1 },
													 "stroke-linecap" => { default => "round" },
													});
	} else {
		%params = @_;
		$params{color}            ||= fetch_conf("default_color") || $default_color;
		$params{"stroke-linecap"} ||= "round";
		$params{thickness}        ||= 1;
	}
    
	if (@{$params{points}} != 4) {
		fatal_error("argument","list_size",current_function(),current_package(),4,int(@{$params{points}}));
	}
    
	my $fmt = "'%.1f'";
	my $style = style(thickness  => $params{thickness},
										color      => $params{color},
										fill_color => undef,
										linecap    => $params{"stroke-linecap"});
	my $svg = sprintf(qq{<line x1=$fmt y1=$fmt x2=$fmt y2=$fmt style="%s"/>},@{$params{points}},$style);
	printdebug_group("svg","line",@{$params{points}},$params{color},$params{thickness},$style);
	Circos::printsvg($svg);
}

sub draw_text {
	my %params;
	my @args = remove_undef_keys(@_);
	if (fetch_conf("debug_validate")) {
		%params = validate( @args, 
												{
												 text      => 1,
												 font      => { default => fetch_conf("default_font") || $default_font },
												 size      => 1,
												 color     => { default => fetch_conf("default_font_color") || $default_font_color },
												 
												 angle     => 1,
												 radius    => 1,
												 
												 angle_offset => { default => 0 },
												 
												 is_parallel => { default => 0 },
												 is_rotated  => { default => 1 },
												 
												 rotation    => { default => 0 },
												 
												 x_offset   => { default => 0 },
												 y_offset   => { default => 0 },
												 
												 guides    => { default => 0 },

												 mapoptions => 0,
												});
	} else {
		%params = @args;
		$params{color}       ||= fetch_conf("default_font_color") || $default_font_color;
		$params{is_parallel} ||= 0;
		$params{x_offset}    ||= 0;
		$params{y_offset}    ||= 0;
	}

	my ( $w, $h )      = get_label_size( text=>$params{text},
																			 size=>$params{size},
																			 font_file=>get_font_file_from_key($params{font}));
	my $font_name      = get_font_name_from_key($params{font});
	my $angle_quadrant = angle_quadrant($params{angle});    
	my $text_angle     = text_angle_2( angle => $params{angle}, 
																		 is_rotated => $params{is_rotated},
																		 is_parallel => $params{is_parallel} );
    
	my $radius_offset  = 0;
	# adjust radius
	if ($params{is_parallel} ) {
		$params{angle_offset} = 0;
		if ($params{is_rotated}) {
	    if ($angle_quadrant == 1 || $angle_quadrant == 2) {
				$radius_offset = $h;
	    }
		}
	}
	# adjust anchor
	my $anchor;
	if ($params{is_parallel}) {
		$anchor       = "middle";
	} else {
		if ($params{is_rotated}) {
	    if ($angle_quadrant <= 1) {
				$anchor = "start";
	    } else {
				$anchor = "end";
	    }
		} else {
	    $anchor = "start";
		}
	}
    
	my ($x,$y) = getxypos( $params{angle}  + $params{angle_offset}, 
												 $params{radius} + $radius_offset );
    
	my $svg_text = $params{text};
	$svg_text =~ s/&/&amp;/g;
    
	my $style = style(thickness  => $params{thickness},
										fill_color => $params{color},
										textanchor => $anchor);
	
	my $svg = sprintf( qq{<text x="%.1f" y="%.1f" font-size="%.1fpx" font-family="%s" style="%s" transform="rotate(%.1f,%.1f,%.1f)">%s</text>},
										 $x,$y,
										 $CONF{svg_font_scale} * $params{size},
										 $font_name,
										 $style,
										 360 - $text_angle,
										 $x,$y,
										 defined $svg_text ? $svg_text : $EMPTY_STR,
									 );
	Circos::printsvg($svg);
}

sub draw_slice {
	$CONF{debug_validate} && validate(@_, {
																				 image        => { isa => 'GD::Image' },
																				 start        => 1,
																				 start_offset => 0,
																				 end_offset   => 0,
																				 end          => 1,
																				 chr          => 1,
																				 radius_from  => 1,
																				 radius_to    => 0,
																				 radius_to_y0 => 0,
																				 radius_to_y1 => 0,
																				 edgecolor    => 1,
																				 edgestroke   => 1,
																				 fillcolor    => 0,
																				 ideogram     => 0,
																				 mapoptions   => { type => HASHREF, optional => 1 },
																				 guides       => 0,
																				 
																				 start_a => 1,
																				 end_a   => 1,
																				 angle_orientation => 1,
																				});
	
	my %params = @_;

	my ($start_a,$end_a,$angle_orientation) = @params{qw(start_a end_a angle_orientation)};
	
	my $style = style(thickness  => $params{edgestroke},
										color      => $params{edgecolor},
										linecap    => $params{linecap} || "round",
										fill_color => $params{fillcolor});
	
	my $svg;
	
	if (defined $params{radius_to} &&
			$params{radius_from} == $params{radius_to} ) {
		my $end_a_mod = $end_a;
		if ( abs( $end_a - $start_a ) > 359.99 || $start_a == $end_a ) {
	    $end_a_mod -= 0.01;
		}
		#
		# when the start/end radius is the same, there can be no
		# fill because the slice is 0-width
		#
		$svg = sprintf(
									 qq{<path d="M %.2f,%.2f A%.2f,%.2f %.2f %d,%d %.2f,%.2f" style="%s"/>},
									 getxypos( $start_a, $params{radius_from} ),
									 $params{radius_from},
									 $params{radius_from},
									 0,
									 abs( $start_a - $end_a_mod ) > 180,
									 1,
									 getxypos( $end_a_mod, $params{radius_from} ),
									 $style,
									);
	} elsif ( $end_a == $start_a ) {
		$svg = sprintf(
									 qq{<path d="M %.2f,%.2f L %.2f,%.2f " style="%s"/>},
									 getxypos( $start_a, $params{radius_from} ),
									 getxypos( $end_a,   $params{radius_to} ),
									 $style,
									);
	} else {
		my $large_arc = abs( $start_a - $end_a ) > 180;
		my $sweep     = $end_a > $start_a ? 1 : 0;
		my $end_a_mod = $end_a;
		if ( abs( $end_a - $start_a ) > 359.99 || $start_a == $end_a ) {
	    $end_a_mod -= 0.01;
		}
		if(defined $params{radius_to_y0} && defined $params{radius_to_y1}) {
			$svg = sprintf(
										 qq{<path d="M%.3f,%.3f A%.3f,%.3f %.3f %d,%d %.3f,%.3f L%.3f,%.3f L%.3f,%.3f Z" style="%s"/>},
										 # move to (M)
										 getxypos( $start_a, $params{radius_from} ),
										 # elliptical arc (A)
										 $params{radius_from}, $params{radius_from}, # rx ry
										 40,                 # x axis rotation
										 $large_arc, $sweep, # large arc flag, sweep flag
										 getxypos( $end_a_mod, $params{radius_from} ), # end x,y
										 getxypos( $end_a_mod, $params{radius_to_y1} ),
										 getxypos( $start_a, $params{radius_to_y0} ),
										 #getxypos( $start_a, $params{radius_to} ),
										 $style,
										);

		} else {
			$svg = sprintf(
										 qq{<path d="M%.3f,%.3f A%.3f,%.3f %.3f %d,%d %.3f,%.3f L%.3f,%.3f A%.3f,%.3f %.3f %d,%d %.3f,%.3f Z" style="%s"/>},
										 # move to (M)
										 getxypos( $start_a, $params{radius_from} ),
										 # elliptical arc (A)
										 $params{radius_from}, $params{radius_from}, # rx ry
										 40,                 # x axis rotation
										 $large_arc, $sweep, # large arc flag, sweep flag
										 getxypos( $end_a_mod, $params{radius_from} ), # end x,y
										 getxypos( $end_a_mod, $params{radius_to} ),
										 $params{radius_to}, $params{radius_to},
										 0,
										 $large_arc, !$sweep, 
										 getxypos( $start_a, $params{radius_to} ),
										 $style,
										);
		}
	}
	Circos::printsvg($svg);
}

################################################################
# Given a hash, generate a style string
#
# key1=value1; key2=value2; key3=value3; ...
sub style_string {
	my %hash = @_;
	my $style = join(";",map { sprintf("%s:%s", $_, $hash{$_}) } grep(defined $hash{$_}, keys %hash));
	return $style;
}

################################################################
# Generate style for
# 
# thickness, color, linecap, fill_color, text-anchor
sub style {
	my %params = @_;
	my @style;
	if (my $an = $params{textanchor}) {
		push @style, sprintf("text-anchor:%s",$an);
	}
	if (my $st = $params{thickness}) {
		push @style, sprintf("stroke-width:%.1f", $st);
		my $sc = $params{color} || fetch_conf("default_color");
		push @style, sprintf("stroke:rgb(%d,%d,%d)", rgb_color($sc));
		if ( (my $op = rgb_color_opacity( $params{color} )) < 1 ) {
	    push @style, sprintf("stroke-opacity:%.2f",$op);
		}
	}
	if (my $lc = $params{linecap}) {
		push @style, sprintf("stroke-linecap:%s",$lc);
	}
	if (my $fc = $params{fill_color}) {
		push @style, sprintf("fill:rgb(%d,%d,%d)", rgb_color($fc));
		if ( (my $op = rgb_color_opacity( $params{fill_color} )) < 1 ) {
			push @style, sprintf("fill-opacity:%.2f",$op);
		}
	} else {
		push @style, "fill:none";
	}
	return join($SEMICOLON,@style) . $SEMICOLON;
}

sub tag {
	my $tag = shift;
	my $tag_data = {
									xml=>     q|<?xml version="1.0" encoding="utf-8" standalone="no"?>|,
									doctype=> q|<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">|,
								 };
	if(! $tag_data->{$tag}) {
		fatal_error("svg","no_such_tag",$tag);
	} else {
		return $tag_data->{$tag};
	}
}

1;
