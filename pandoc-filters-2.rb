# Author: D F A Ligertwood <dfaligertwood@dunelm.org.uk>
# License: BSD3
#
# Port of pandocfilters.py by John MacFarlane, available at
# github.com/jgm/pandocfilters
#
# Functions to aid writing Ruby scripts that process the pandoc
# AST serialized as JSON.

module Pandoc
    module Methods
	def self.pandoc_map(*a) self end
	def self.walk &action
	    self.pandoc_map { |object| object.walk &action }
	end
	def self.owalk &action
	    self.pandoc_map { |object| object.owalk &action }
	end
    end

    class Hash < Object::Hash
	include Methods
	def pandoc_map(&block)
	    map { |k,v| [k, block.(v)] }.to_h
	end
	def pandoc
	    if self['t'] and self['c']
		return true
	    else
		return false
	    end
	end
    end

    class Array < Object::Array
	include Methods
	def pandoc_map(&block)
	    a = Array.new
	    each { |item|
		result = block.(item)
		if result.is_a? Array and not item.is_a? Array
		    a += result
		else
		    a << result
		end
	    }
	    a
	end
    end

    def Document < Array
	def meta
	    document[0]["unMeta"]
	end
	def contents
	    document[1]
	end
	def pandoc_map &b
	    self.new([{"unmeta" => self.meta}, b.(self.contents)])
	end
    end
end
