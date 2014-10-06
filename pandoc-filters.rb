# Author: D F A Ligertwood <dfaligertwood@dunelm.org.uk>
# License: BSD3

# Port of pandocfilters.py by John MacFarlane, available at
# github.com/jgm/pandocfilters
#
# Functions to aid writing Ruby scripts that process the pandoc
# AST serialized as JSON.

require 'json'

class Object
	def to_pandoc(meta)
		self
	end

	def walk(action)
		self
	end
end

class Hash
	def to_pandoc(meta)
		if self['t']
			Pandoc.new(self['t'], self['c'].to_pandoc(meta), meta)
		else
			a = self
			a.each { |k,v|
				a[k] = v.to_pandoc(meta)
			}
		end
	end
end

class Array
	def to_pandoc(meta)
		self.map { |item|
			item.to_pandoc(meta)
		}
	end

	def walk(action)
		self.map { |item|
			item.walk(action)
		}
	end
end

class Pandoc
	attr :type, :contents, :meta
	def initialize(type, contents, meta)
		@format = if ARGV.length > 1
				  ARGV[1]
			  else
				  ""
			  end
		@type = type
		@contents = contents
		@meta = meta
	end

	def self.new_document
		document = JSON.load(STDIN)
		meta = document[0]["unMeta"]
		contents = document[1].to_pandoc(meta)
		type = "Document" 
		Pandoc.new(type, contents, meta)
	end

	def walk(action)
		result = self.send action
		if result
			return result.walk(action)
		else
			return @contents.walk(action)
		end
	end
	
	def to_hash
		hash = Hash.new
		hash['t'] = self.type
		hash['c'] = self.contents
		hash
	end
	alias to_h to_hash

	def to_json(*args)
		if @type.to_s == "Document"
			document = Array.new
			document[0] = Hash.new
			document[0]["unMeta"] = @meta
			document[1] = @contents
			return JSON.dump(document)
		else
			return JSON.dump(self.to_hash)
		end
	end

	def self.toJSONFilter(action)
		document = Pandoc.new_document
		document.walk(action)
		$stdout.print document.to_json
	end
end

# Stringify function. Example of how to define actions.

class Pandoc
	@@stringify_result = String.new

	def stringify
		@@stringify_result = ""
		self.walk :stringify_action
		return @@stringify_result
	end
	alias to_s stringify

	def stringify_action
		case @type
		when "Str"
			@@stringify_result << @contents.to_s
		when "Code", "Math"
			@@stringify_result << @contents[1].to_s
		when "LineBreak", "Space"
			@@stringify_result << " "
		end
		false
	end
end

# Constructors.

class Pandoc
	private
	def self.elt(eltType, numargs, *args)
		raise ArgumentError, "#{eltType} expects #{numargs} arguments, \
			but received #{args.length} arguments" unless numargs == args.length
		if numargs == 1
			return Pandoc.new(eltType, args[0], {})
		else
			return Pandoc.new(eltType, args, {})
		end
	end

	public
	class <<self
		# Block Elements

		def Plain(*args)
			elt('Plain', 1, *args)
		end
		def Para(*args)
			elt('Para', 1, *args)
		end
		def CodeBlock(*args)
			elt('CodeBlock', 2, *args)
		end
		def RawBlock(*args)
			elt('RawBlock', 2, *args)
		end
		def BlockQuote(*args)
			elt('BlockQuote', 1, *args)
		end
		def OrderedList(*args)
			elt('OrderedList', 2, *args)
		end
		def BulletList(*args)
			elt('BulletList', 1, *args)
		end
		def DefinitionList(*args)
			elt('DefinitionList', 1, *args)
		end
		def Header(*args)
			elt('Header', 3, *args)
		end
		def HorizontalRule(*args)
			elt('HorizontalRule', 0, *args)
		end
		def Table(*args)
			elt('Table', 5, *args)
		end
		def Div(*args)
			elt('Div', 2, *args)
		end
		def Null(*args)
			elt('Null', 0, *args)
		end

		# Inline Elements

		def Str(*args)
			elt('Str', 1, *args)
		end
		def Emph(*args)
			elt('Emph', 1, *args)
		end
		def Strong(*args)
			elt('Strong', 1, *args)
		end
		def Strikeout(*args)
			elt('Strikeout', 1, *args)
		end
		def Superscript(*args)
			elt('Superscript', 1, *args)
		end
		def Subscript(*args)
			elt('Subscript', 1, *args)
		end
		def SmallCaps(*args)
			elt('SmallCaps', 1, *args)
		end
		def Quoted(*args)
			elt('Quoted', 2, *args)
		end
		def Cite(*args)
			elt('Cite', 2, *args)
		end
		def Code(*args)
			elt('Code', 2, *args)
		end
		def Space(*args)
			elt('Space', 0, *args)
		end
		def LineBreak(*args)
			elt('LineBreak', 0, *args)
		end
		def Math(*args)
			elt('Math', 2, *args)
		end
		def RawInline(*args)
			elt('RawInline', 2, *args)
		end
		def Link(*args)
			elt('Link', 2, *args)
		end
		def Image(*args)
			elt('Image', 2, *args)
		end
		def Note(*args)
			elt('Note', 1, *args)
		end
		def Span(*args)
			elt('Span', 2, *args)
		end
	end
end
