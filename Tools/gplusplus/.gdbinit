# Macros for handling g++ data types

# strings

# Data in string is a this->_M_dataplus._M_p
# Length of string is _M_rep()->_M_length
# 
#       _Rep*
#       _M_rep() const
#       { return &((reinterpret_cast<_Rep*> (_M_data()))[-1]); }
# 
#       _CharT*
#       _M_data() const
#       { return  _M_dataplus._M_p; }


# Arg0 is a pointer to a list; arg1 is the type of the list elements
# Example:
# dumpList &dg->m_metaDevSoftPtrList SoftPointer*
define dumpList 
    set $headptr = (_List_node_base *) ($arg0)->_M_node
    if $headptr == $headptr->_M_next
	echo List is empty\n
    else
	set $curPtr = $headptr->_M_next
	set $i = 0
	while ($curPtr != $headptr)
	    set $realPtr = ((_List_node<$arg1>*) ($curPtr))->_M_data
	    echo ==List element #
	    output $i
	    echo (
	    output $realPtr
	    echo )\n
	    p  *((_List_node<$arg1>*) ($curPtr))->_M_data
	    set $curPtr = $curPtr->_M_next
	    set $i = $i + 1
        end
    end
end

document dumpList
Dump the contents of a C++ list.  Args: pointer to list, type of list elements
end
